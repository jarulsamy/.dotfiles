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
                 :family "Hack"
                 :size font-size
                 :weight 'normal)
      doom-variable-pitch-font (font-spec
                                :family "Hack"
                                :size font-size
                                :weight 'normal)
      doom-theme 'doom-dark+
      display-line-numbers-type 'relative)

;;; Dashboard
(setq +doom-dashboard-ascii-banner-fn #'dashboard-random-ascii-banner)

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
      evil-want-fine-undo nil)
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
                                "-j=6"
                                "--pch-storage=memory"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;;; Org

;; Agenda
(custom-set-variables
 '(org-agenda-custom-commands
   '(("t" "TODO Agenda"
      ((agenda ""))
      ((org-agenda-tag-filter-preset '("-SCHEDULE")))))))

(setq org-agenda-start-with-follow-mode t
      org-agenda-restore-windows-after-quit t
      org-agenda-timegrid-use-ampm t)

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                            :post (setq which-key-inhibit nil)
                            :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(map! :after org-agenda
      :map evil-normal-state-map
      "<f9>" #'hydra-org-agenda/body)

(use-package! smart-compile)

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
  (setq magit-diff-refine-hunk 'all)
  (setq magit-repository-directories '(("~/repos"     . 1)
                                       ("~/workRepos" . 1))))

;;; Projectile
(setq projectile-files-cache-expire 30
      projectile-sort-order 'recentf
      projectile-project-search-path '(("~/repos"     . 1)
                                       ("~/workRepos" . 1))
      projectile-per-project-compilation-buffer t)

;;; Workspaces
(setq perp-autokill-buffer-on-remove t)

;;; Latex
(setq +latex-viewers '(pdf-tools))
;; Use pdflatex for tex
(setq smart-compile-alist (add-to-list 'smart-compile-alist '("\\.tex\\'" . "pdflatex %f")))

;; Dired
(after! dired
  (setq-default dired-listing-switches "-ABDgGlX --group-directories-first"))
(map! :leader
      :mode :n
      :after dired
      (:desc "Dired Split" "o _" #'dired-jump-other-window))

;; Bring back ctrl-a/x
(after! evil
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

  (define-key evil-normal-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (define-key evil-normal-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

;; Debugging
(after! dap-mode
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb)
  (require 'dap-lldb)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (dap-ui-mode nil)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode nil))

;; Spotify
(map! :leader
     :after spotify
     :prefix "S"
     "SPC" #'spotify-playpause)
(map! :leader
     :after spotify
     :prefix "S"
     "n" #'spotify-next)
(map! :leader
     :after spotify
     :prefix "S"
     "p" #'spotify-previous)
