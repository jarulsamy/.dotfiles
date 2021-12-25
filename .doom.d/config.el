;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Joshua Arulsamy"
      user-mail-address "joshua.gf.arul@gmail.com")

;; Cascadia Code no longer has poor scrolling performance issues!
(setq doom-font (font-spec :family "Cascadia Code" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Cascadia Code" :size 14))

(setq doom-theme 'doom-gruvbox)
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq-default delete-by-moving-to-trash t)

(setq undo-limit 80000000
      evil-want-fine-undo t)

(display-time-mode 1)

;; Ascii Banner
(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '("     _           _"
            "    | | ___  ___| |__"
            " _  | |/ _ \\/ __| '_ \\"
            "| |_| | (_) \\__ | | | |"
            " \\___/ \\___/|___|_| |_|"))
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
(setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn)

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

;; Keybinds
;; Quickly switch between corresponding files
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

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
(global-wakatime-mode)
(setq-default wakatime-cli-path "/home/joshua/.wakatime/wakatime-cli-linux-amd64")

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
(setq org-roam-directory "~/repos/brain")

;;; config.el ends here
