;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Joshua Arulsamy"
      user-mail-address "joshua.gf.arul@gmail.com")

;; Cascadia Code no longer has poor scrolling performance issues!
(setq doom-font (font-spec :family "Cascadia Code" :size 16 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Cascadia Code" :size 16))

(setq doom-theme 'doom-gruvbox)
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Use system trash bin.
(setq-default delete-by-moving-to-trash t)
(setq undo-limit 80000000 )
(display-time-mode 1)

;; Source: https://stackoverflow.com/a/94277/8846676
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p)
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
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

;; Source: First elisp function I ever wrote entirely by myself! :D
(defun file-contents-to-list (filename)
  (let ((contents (with-temp-buffer
                    (insert-file-contents filename)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (split-string contents "\n")))

;; Pick a random item from a list
(defun random-choice (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

;; Pick a random file from `directory'.
(defun random-txt-file (directory)
  (random-choice (directory-files (expand-file-name directory) 'full
                                  (rx ".txt" eos))))

;; Location for banners.
(defcustom banner-directory "~/.doom.d/banners/"
  "Directory to search for .txt files for doom dashboard banner.")

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

;; Doom-Modeline
(setq doom-modeline-project-detection 'auto
      doom-modeline-buffer-file-name-style 'auto
      doom-modeline-icon (display-graphic-p)
      doom-modeline-major-mode-color-icon t
      doom-modeline-buffer-state-icon t
      doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
      doom-modeline-buffer-encoding t
      doom-modeline-indent-info nil
      doom-modeline-checker-simple-format t
      doom-modeline-number-limit 99
      doom-modeline-vcs-max-length 20
      doom-modeline-workspace-name t
      doom-modeline-persp-name t
      doom-modeline-display-default-persp-name nil
      doom-modeline-persp-icon t
      doom-modeline-lsp t
      doom-modeline-modal-icon t
      doom-modeline-buffer-modification-icon t
      doom-modeline-unicode-fallback nil
      doom-modeline-minor-modes nil
      doom-modeline-enable-word-count nil)

;;; Modules
;;; :editor evil
;; Focus new window after splitting
(setq evil-split-below t
      evil-vsplit-window-right t)

;; Shamelessly stolen from: https://tecosaur.github.io/emacs-config/config.html
(setq which-key-idle-delay 0.5) ;; I need the help, I really do

;; Having evil everywhere is a little verbose:
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

;; Wakatime
(setq-default wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli-linux-amd64"))
(global-wakatime-mode)

;; Clangd LSP
(setq lsp-clients-clangd-args '("--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"
                                "-j=4"
                                "--pch-storage=memory"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; Org-roam
(setq org-roam-directory "~/org/brain")
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Twittering
(setq twittering-allow-insecure-server-cert t)
(setq twittering-icon-mode t)
(setq twittering-use-icon-storage t)

;; Default python environment
(after! pyvenv
  (pyvenv-workon "base"))

;; Magit
(after! magit
  (setq magit-diff-refine-hunk 'all))

;; Evil Mode
(setq evil-move-cursor-back t
      evil-want-fine-undo t)

;; Bring back s/S
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; Email
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


;; Compiling
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn (delete-windows-on buffer)))
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame)
  )

;; Bury the compilation buffer when compilation is done/successful.
(add-to-list 'compilation-finish-functions
             'notify-compilation-result)
