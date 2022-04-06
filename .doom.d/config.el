;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; General
(setq user-full-name "Joshua Arulsamy"
      user-mail-address "joshua.gf.arul@gmail.com")

(defun primary-display-height ()
  "Get the height of the current display this frame is on."
  (interactive)
  (nth 4 (assoc 'geometry (car (display-monitor-attributes-list)))))

(defun primary-display-width ()
  "Get the width of the current display this frame is on."
  (interactive)
  (nth 3 (assoc 'geometry (car (display-monitor-attributes-list)))))

;; Source: https://stackoverflow.com/a/94277/8846676
(defun set-frame-size-according-to-resolution ()
  "Automatically adjust the default frame size accoring to display resolution."
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (primary-display-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 400)
                                      (frame-char-height)))))))
(set-frame-size-according-to-resolution)

;;; Themeing
;; Larger font for higher res display.
;; New 4K XPS 15 :D
(setq font-size 16)
(if (display-graphic-p)
    (progn
      (when (> (primary-display-width) 3400)
          (setq font-size 24))))

;; Cascadia Code no longer has poor scrolling performance issues.
(setq doom-font (font-spec
                 :family "Jetbrains Mono"
                 :size font-size
                 :weight 'normal)
      doom-variable-pitch-font (font-spec
                                :family "Jetbrains Mono"
                                :size font-size
                                :weight 'normal)
      doom-theme 'doom-one
      display-line-numbers-type 'relative)

;; Use system trash bin.
(setq-default delete-by-moving-to-trash t)
(setq undo-limit 80000000 )

;; Time Formatting
(setenv "TZ" "America/Denver")
(setq display-time-format "%l:%m:%S %p"
      display-time-interval 1
      display-time-default-load-average nil)
;; TODO: This is almost always the incorrect time...Investigate.
(display-time-mode 0)


;;; Custom Banners
(defcustom banner-directory "~/.doom.d/banners/"
  "Directory to search for .txt files for doom dashboard banner.")

;; Source: First elisp function I ever wrote entirely by myself! :D
(defun file-contents-to-list (filename)
  (let ((contents (with-temp-buffer
                    (insert-file-contents filename)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (split-string contents "\n")))

(defun random-choice (items)
  "Pick a random item from a list."
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun random-txt-file (directory)
  "Pick a random file from `directory'."
  (random-choice (directory-files (expand-file-name directory) 'full
                                  (rx ".txt" eos))))

(defun doom-dashboard-banner-fn ()
  (let* ((banner (file-contents-to-list (random-txt-file banner-directory)))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))
(setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-banner-fn)

;;; Battery TODO: This doesn't ever update...
(use-package! battery
  :config
  ;; Only show battery symbol if we can successfully read the status.
  (let ((battery-status (battery-format "%B" (funcall battery-status-function))))
    (setq display-battery-mode
          (not
           (or (string-match-p "N/A" battery-status)
               (string-match-p "unknown" battery-status))))))

;;; Modeline
(after! doom-modeline
  ;; Time segment
  (doom-modeline-def-segment time-segment
    "Mode line construct for current time."
    (when (and (doom-modeline--active)
               (not doom-modeline--limited-width-p))
      '("" display-time-string)))

  ;; Set modeline segments and fix oversized icons being cutoff on the right
  ;; side.
  (doom-modeline-def-modeline 'main
    '(hud bar matches input-method buffer-info-simple remote-host buffer-position)
    '(minor-modes persp-name lsp checker vcs time-segment battery "  "))

  ;; Misc customization
  (setq doom-modeline-hud t
        doom-modeline-buffer-modification-icon t
        doom-modeline-project-detection 'auto
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-env-version t
        doom-modeline-icon (display-graphic-p)
        doom-modeline-lsp t
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 32
        doom-modeline-workspace-name t
        doom-modeline-persp-name t
        doom-modeline-persp-icon t
        doom-modeline-mu4e t
        doom-modeline-unicode-fallback nil
        doom-modeline-display-default-persp-name nil
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t))

;;; Evil
(setq evil-split-below t
      evil-vsplit-window-right t
      evil-move-cursor-back t
      evil-want-fine-undo t)
;; Bring back s/S
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;;; Which-key
;; Having evil everywhere is a little verbose:
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
(setq which-key-idle-delay 0.5)

;;; Company
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-quick-access t)
  ;; Make aborting less annoying.
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

;;; Wakatime
(setq-default wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli-linux-amd64"))
(if (file-executable-p wakatime-cli-path)
    (global-wakatime-mode 1)
  (error "Wakatime CLI not found here (%s)" wakatime-cli-path))

;;; Clangd LSP
(setq lsp-clients-clangd-args '("--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"
                                "-j=4"
                                "--pch-storage=memory"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;;; Org(-roam)
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/brain")

;; Org Agenda
(custom-set-variables
 '(org-agenda-custom-commands
   '(("t" "TODO Agenda"
      ((agenda ""))
      ((org-agenda-tag-filter-preset '("-SCHEDULE")))))))

;;; Org-roam-ui
(use-package! websocket
  :after org-roam)
(use-package! org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;; Twittering
(setq twittering-allow-insecure-server-cert t)
(setq twittering-icon-mode t)
(setq twittering-use-icon-storage t)

;;; Pyvenv
(after! pyvenv
  (pyvenv-workon "base"))

;;; Magit
(after! magit
  (setq magit-diff-refine-hunk 'all))

;;; Email
(after! mu4e
  (setq!
   mu4e-update-inverval 1800
   mu4e-headers-leave-behavior 'apply
   mu4e-use-fancy-chars t
   mu4e-view-show-addresses t
   mu4e-view-show-images t
   mu4e-maildir-shortcuts
   '( ("/Inbox" . ?i)
      ("/Archive" . ?a)
      ("/Drafts" . ?d)
      ("/Deleted Items" . ?t))
   ))

(set-email-account! "UW"
                    '((mu4e-sent-folder              . "/Sent Items")
                      (mu4e-drafts-folder            . "/Drafts")
                      (mu4e-trash-folder             . "/Deleted Items")
                      (mu4e-refile-folder            . "/Archive")
                      (mu4e-compose-signature        . "")
                      (mu4e-get-mail-command         . "mbsync work")
                      (smtpmail-stream-type          . 'starttls)
                      (user-mail-address             . "jarulsam@uwyo.edu")
                      (smtpmail-smtp-user            . "jarulsam@uwyo.edu")
                      (smtpmail-smtp-server          . "smtp.office365.com")
                      (smtpmail-smtp-service         . 587)
                      )
                    t)

;;; Compiling
(use-package! smart-compile)
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,close the *compilation* buffer
If the compilation is successful,and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn (delete-windows-on buffer)))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame))
(add-to-list 'compilation-finish-functions
             'notify-compilation-result)

;; Use pdflatex for tex
(setq smart-compile-alist (add-to-list 'smart-compile-alist '("\\.tex\\'" . "pdflatex %f")))

;;; Projectile
(setq projectile-project-search-path '(("~/repos"     . 1)
                                       ("~/workRepos" . 1)))

;;; Latex
(setq +latex-viewers '(pdf-tools))

;; Disable exit confirmation
(setq confirm-kill-emacs nil)
