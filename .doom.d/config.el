;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
;;; Josh's Magical Emacs Configuation
;;; I've spent too much time on this...
;;;

;;; Bootup/Initialization

;; Prepend custom load path
(setq load-path (cons "~/.doom.d/libs" load-path))

;; Load custom libraries
(load-library "display")
(load-library "dashboard")
(load-library "compUtils")

;;; General
(setq user-full-name "Joshua Arulsamy"
      user-mail-address "joshua.gf.arul@gmail.com")

;; Use system trash bin.
(setq-default delete-by-moving-to-trash t)
(setq undo-limit 160000000) ;; 1000x default

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; Timezone
(setenv "TZ" "America/Denver")

;; Frame/Font Sizing
(display-set-frame-size-according-to-resolution)
(setq font-size 16)
(if (display-graphic-p)
    (progn
      (when (> (display-primary-width) 3400)
        (setq font-size 24))))

;;; Font/Theme
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

;;; Dashboard
(setq +doom-dashboard-ascii-banner-fn #'dashboard-random-ascii-banner)

;;; Time Formatting
(setq display-time-format "%I:%M:%S %p"
      display-time-interval 1
      display-time-default-load-average nil)
(display-time-mode 1)

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

  ;; Set segments and fix oversized icons being cutoff on the right side.
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

;;; Org

;; Agenda
(custom-set-variables
 '(org-agenda-custom-commands
   '(("t" "TODO Agenda"
      ((agenda ""))
      ((org-agenda-tag-filter-preset '("-SCHEDULE")))))))

;; Roam
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/brain")

;; Roam-ui
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

;;; Pyvenv
(after! pyvenv
  (pyvenv-workon "base"))

;;; Magit
(after! magit
  (setq magit-diff-refine-hunk 'all))

;;; Misc Compiling
(use-package! smart-compile)
(add-to-list 'compilation-finish-functions 'notify-compilation-result)

;;; Projectile
(setq projectile-project-search-path '(("~/repos"     . 1)
                                       ("~/workRepos" . 1)))
;;; Latex
(setq +latex-viewers '(pdf-tools))
;; Use pdflatex for tex
(setq smart-compile-alist (add-to-list 'smart-compile-alist '("\\.tex\\'" . "pdflatex %f")))

;; Dired
(after! dired
  (setq-default dired-listing-switches "-ABDgGlX --group-directories-first"))

;; Equivalent of nvim-treesitter-context
;; (which-function-mode 1)
;; (setq idle-update-delay 0.25)

;; (setq which-func-header-line-format '(which-func-mode ("" which-func-format)))
;; (defadvice which-func-ff-hook (after header-line activate)
;;   (when which-func-mode
;;     (setq mode-line-format (delete (assoc 'which-func-mode
;;                                           mode-line-format) mode-line-format)
;;           header-line-format which-func-header-line-format)))

;; Bring back ctrl-a/x
(after! evil
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

  (define-key evil-normal-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (define-key evil-normal-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

(defun xdg-open (url)
  "Open a URL with the default application."
  (print url)
  (call-process "xdg-open" nil 0 nil url))

(defun open-project-root ()
  "Open a doom project with xdg-open."
  (interactive)
  (xdg-open (expand-file-name (expand-file-name (doom-project-root) (expand-file-name "~")))))

(define-key evil-normal-state-map (kbd "g C-o") 'open-project-root)
