;; Emacs init file 2026.09.13 JMT
;;
;; Single shared config for Windows and Linux (WSL).
;;   Hard copy lives in the myEmacs git repo (C:/Users/JohnM/myEmacs on Windows).
;;   Windows: %APPDATA%/.emacs.d/init.el is a one-line stub that loads this file.
;;   WSL:     ~/.emacs.d/init.el is a symlink to /mnt/c/Users/JohnM/myEmacs/init.el
;; Anything machine-specific goes in the platform blocks near the bottom.

(defconst jmt/windows-p (eq system-type 'windows-nt) "Non-nil on native Windows Emacs.")
(defconst jmt/linux-p   (eq system-type 'gnu/linux)  "Non-nil on Linux / WSL Emacs.")

;;===================================================================
;; package managers
;;===================================================================
(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Keep Custom's auto-written settings out of this shared file.
;; Each machine gets its own custom.el next to the (stub or symlinked) init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;garbage collection setting
(setq gc-cons-threshold 64000000)

;;===================================================================
;; tweaks and minor settings
;;===================================================================
;;theme
(load-theme 'misterioso t)
;;font -- Cascadia Mono 12pt (same as Windows Terminal).
;; On Linux: sudo apt install fonts-cascadia-code
(when (member "Cascadia Mono" (font-family-list))
  (set-face-attribute 'default nil :family "Cascadia Mono"))
(set-face-attribute 'default nil :height 120)
; Set cursor color
(set-cursor-color "#ff88ff") ;;pinkish
;;tab modes nil
(setq-default indent-tabs-mode nil)
;;# of spaces for tab
(setq tab-width 3)
;;highlight color
(set-face-attribute 'region nil :background "#338F86")
;;highlight mathing parenthisis
(show-paren-mode t)
;; Overwrite region selected
(delete-selection-mode t)
;; Show column numbers by default
(setq column-number-mode t)
;; Prevent emacs from creating a backup file like: filename~
(setq make-backup-files nil)
;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilight matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode t)
;;line highlight color
(set-face-background 'hl-line "#2D4948")
;; small interface tweaks
(setq inhibit-startup-message t)
;;no line wrapping
(set-default 'truncate-lines t)
;;disable tool bar
(tool-bar-mode -1)
;;disable menu bar
(menu-bar-mode -1)
;; I don't care to see the splash screen
(setq inhibit-splash-screen t)
;; Hide the scroll bar
(scroll-bar-mode -1)
;;cursor never stop blinking
(setq blink-cursor-blinks 0)
;; Enable line numbers on the LHS
(global-display-line-numbers-mode 0)
;; Don't ring the bell
(setq ring-bell-function 'ignore)
;;y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; Disable the horrid auto-save
(setq auto-save-default nil)
;;use side scroller to scroll horizontally
(setq mouse-wheel-tilt-scroll 1)
;;save cursor position between sessions
(save-place-mode 1)
;;keep buffer up to date if file changes outside emacs
(global-auto-revert-mode t)
;;title bar shows full path
(setq-default frame-title-format '("%b"))
;; autopair
(electric-pair-mode 1)
(setq electric-indent-mode nil)
(setq standard-indent 2)
(setq whitespace-line-column 90)

;; Smooth out the scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; # line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;; grep / ediff defaults
(setq grep-command "grep --exclude-dir={Isrc} --color=always -s -n -e 2>/dev/null")
(setq ediff-diff-options "--binary -w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq warning-suppress-types '((mule)))

;; disable VC (git) integration -- it makes remote (tramp) editing crawl
(setq vc-handled-backends nil)

;;===================================================================
;; Global Keyboard Shortcuts
;;===================================================================
;;for general purpose, assigned macros to these
(global-set-key (kbd "<f5>") 'ffive)
(global-set-key (kbd "<f6>") 'fsix)
(global-set-key (kbd "<f7>") 'fseven)
(global-set-key (kbd "<f8>") 'feight)

;; Cycle through line-end display modes
(defun cycle-through-linum-modes ()
  "Cycle the current buffer's line display:
truncate (code) -> visual-line (word-wrapped prose) -> plain wrap -> truncate."
  (interactive)
  (cond
   ;; truncate -> visual-line
   (truncate-lines
    (setq truncate-lines nil)
    (visual-line-mode 1)
    (message "Line display: visual-line (word wrap)"))
   ;; visual-line -> plain wrap
   (visual-line-mode
    (visual-line-mode -1)
    (setq truncate-lines nil)
    (message "Line display: wrap"))
   ;; plain wrap -> truncate
   (t
    (setq truncate-lines t)
    (message "Line display: truncate")))
  (force-mode-line-update))
(global-set-key (kbd "<f9>") 'cycle-through-linum-modes)
;; Show whitespace
(global-set-key (kbd "<f10>") 'whitespace-mode)
;;copy file path of current buffer to clip board
(global-set-key (kbd "C-c C-/") 'er-copy-file-name-to-clipboard)
;; Easy undo key
(global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region) ; terminal fallback (C-; can't be sent)
;;open containing folder
(global-set-key (kbd "C-c C-f") 'browse-file-directory)
;;exand region
(global-set-key (kbd "M-m") 'er/expand-region)
;;mouse keys
(global-set-key (kbd "<mouse-4>") 'next-buffer)
(global-set-key (kbd "<mouse-5>") 'previous-buffer)
;; 'previous' frame to capital 'O'
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

(add-to-list 'auto-mode-alist '("\\.px\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))

;;===================================================================
;; packages
;;===================================================================
;; key binding guide
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; expand region
(use-package expand-region
  :ensure t)

;; highlight mode
(use-package idle-highlight-mode
  :ensure t)
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; better search
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

;;ivy for help with M-x commands
(use-package ivy
  :ensure t
  :config (ivy-mode t))

;;===================================================================
;; tramp for remote editing
;;===================================================================
(setq tramp-default-method "ssh")               ; native OpenSSH instead of plink
(setq tramp-default-user "johton2u")            ; remote account differs from local login
(setq tramp-verbose 1)
(setq remote-file-name-inhibit-locks t
      remote-file-name-inhibit-auto-save-visited t)

;; Windows Emacs cannot allocate a pty, so ssh does not request a remote tty
;; ("Pseudo-terminal will not be allocated because stdin is not a terminal").
;; Without a tty the remote bash starts non-interactive, PS1 is empty, and no
;; prompt is ever printed -- so Tramp waits forever for one.  plink allocated a
;; pty on its own, which is why it always worked.  Force ssh to do the same.
(when jmt/windows-p
  (with-eval-after-load 'tramp-sh
    (let* ((method (assoc "ssh" tramp-methods))
           (cell   (assq 'tramp-login-args (cdr method))))
      (unless (member '("-tt") (cadr cell))
        (setf (cadr cell) (cons '("-tt") (cadr cell)))))))

(with-eval-after-load 'tramp
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp tramp-file-name-regexp)))

;;===================================================================
;; Custom Functions
;;===================================================================
;; Open file's containing folder
(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

;;grab file path from butter
(defun er-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "buffer path '%s'" filename))))

;;===================================================================
;; keep emacs demon running for fast startups
;;===================================================================
(require 'server)
(if (not (server-running-p)) (server-start))

;;===================================================================
;; header line
;;===================================================================
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun sl/make-header ()
  ""
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]"))
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "#727b9e"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat (with-face sl/header
                             ;; :background "red"
                             :foreground "#2D3743"
                             :weight 'bold
                             )))
      (concat (with-face sl/header
                         ;; :background "green"
                         ;; :foreground "black"
                         :weight 'bold
                         :foreground "#2D3743"
                         )
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  (if (buffer-file-name)
      (setq header-line-format
            '("" ;; invocation-name
              (:eval (if (buffer-file-name)
                         (sl/make-header)
                       "%b"))))))

(add-hook 'buffer-list-update-hook
          'sl/display-header)

;;=========================================================================================
;; Siemens Customization
;;=========================================================================================
(defun johton2u-connect ()
  (interactive)
  (find-file "/ssh:orw-johton2u-r8.wv.mentorg.com:/wv/johton2u"))

(defun lmweb-connect ()
  (interactive)
  (find-file "/ssh:orw-lmweb-r8.wv.mentorg.com:/wv/johton2u"))

(defun icbuild-connect ()
  (interactive)
  (find-file "/ssh:icbuild@icbuild-login:/wv/callic"))

;;=========================================================================================
;; Platform specific
;;=========================================================================================
(when jmt/windows-p
  ;; use Git-for-Windows diff for ediff
  (let ((git-diff "C:\\Program Files\\Git\\usr\\bin\\diff.exe"))
    (when (file-exists-p git-diff)
      (setq ediff-diff-program git-diff)))

  (defvar wsl-home "//wsl$/Ubuntu-24.04/home/jmtoniolo"
    "Home directory on the WSL Ubuntu-24.04 distro, reached over the wsl$ UNC share.")

  (defun wsl-connect ()
    "Open dired on my WSL Ubuntu-24.04 home directory."
    (interactive)
    (let ((default-directory (file-name-as-directory wsl-home)))
      (dired default-directory))))

(when jmt/linux-p
  ;; nothing Linux-only yet
  )

;;=========================================================================================
;; Open notes on startup (whichever machine we're on)
;;=========================================================================================
(dolist (notes-file '("c:/Users/JohnM/scratch/notes.md"
                      "c:/Users/z004ka2x/OneDrive - Siemens AG/Documents/personal/Project_Notes.org"))
  (when (file-exists-p notes-file)
    (find-file notes-file)))
;; END=========================================================================================
