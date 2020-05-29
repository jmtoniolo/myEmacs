;; Emacs init file 2020:05:25

;;===================================================================
;; package managers
;;===================================================================
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;garbage collection setting
(setq gc-cons-threshold 64000000)


;;===================================================================
;; tweaks and minor settings
;;===================================================================
;;theme
(load-theme 'misterioso)
; Set cursor color to white
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
;; Set default window size and position
(setq default-frame-alist
      '(;;(top . 0) (left . 0) ;; position
        (width . 100) (height . 38) ;; size
        ))
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
;; Compilation command for C/C++
(setq compile-command "g++ -std=c++14 ")
;;;; Global Keyboard Shortcuts ;;;;;;;;;;;;;;;;;;;
;; Show whitespace
(global-set-key (kbd "<f9>") 'whitespace-mode)
;;show whitespace
(global-set-key (kbd "<f10>") 'treemacs)
;;minimap toggle
(global-set-key (kbd "<f12>") 'minimap-mode)
;;copy file path of current buffer to clip board
(global-set-key (kbd "C-c C-/") 'er-copy-file-name-to-clipboard)
;; Easy undo key
(global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;;open containing folder
(global-set-key (kbd "C-c C-f") 'browse-file-directory)
;;exand region
(global-set-key (kbd "M-m") 'er/expand-region)
;;better search
(global-set-key (kbd "C-s") 'swiper)
;;mouse keys
(global-set-key (kbd "<mouse-4>") 'next-buffer)
(global-set-key (kbd "<mouse-5>") 'previous-buffer)
;;use side scroller to scroll horizontally
(setq mouse-wheel-tilt-scroll 1)
;;save cursor position between sessions
(save-place-mode 1)
;;keep buffer up to date if file changes outside emacs
(global-auto-revert-mode t)
;;title bar shows full path
(setq-default frame-title-format '("%b"))


;; Smooth out the scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; # line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 3
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Dassault Style, 2 spaces, left brace under function and allied left
(c-add-style "DassaultTwo"
	     '("bsd"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 2)))


;; Dassault Style, 3 spaces, left brace under function and allied left
(c-add-style "DassaultThree"
	     '("bsd"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 3)))

;; Dassault Style, 4 spaces, left brace under function and allied left
(c-add-style "DassaultFour"
	     '("bsd"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)))

;; My Style, 3 spaces, left brace after function.
(c-add-style "JMT"
	     '("k&r"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 3)))

;; My Style, 3 spaces, left brace after function.
(c-add-style "DassaultTab"
	     '("bsd"
	       (indent-tabs-mode . t)        ; use tabs
	       (tab-width . 4)))

(defun my-c++-mode-hook ()
  (c-set-style "JMT"))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)


;;===================================================================
;; packages
;;===================================================================

;; key binding guide
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; simple autocomplete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

;; expand region
(use-package expand-region
  :ensure t)

;; highlight mode
(use-package idle-highlight-mode
  :ensure t)
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; better search
(use-package swiper
  :ensure t)

;; autopair
(use-package autopair
  :ensure t
  :diminish autopair-mode
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function autopair-global-mode "autopair.el"))
  :config
  (autopair-global-mode t))

;;hide minor modes
(use-package diminish
  :ensure t)
(diminish 'ivy-mode)
(diminish 'minimap-mode)
(diminish 'which-key-mode)

;;ivy for help with M-x commands
(use-package ivy
  :ensure t
  :config (ivy-mode t))

;;minimap on right side
(use-package minimap
  :ensure t
  :config
          (setq minimap-recenter-type 'middle)
          (setq minimap-window-location 'right))
 
;;treemacs <3
(use-package treemacs
  :ensure t)

;;lua mode
(use-package lua-mode
  :ensure t)

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
;; HTML Tuning
;;===================================================================
;;html settings
   (add-hook 'html-mode-hook
        (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
          (set (make-local-variable 'sgml-basic-offset) 4)
	  (setq-default indent-tabs-mode nil)))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
;;(add-to-list 'auto-mode-alist '("\\.css$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.cfm$" . html-mode))


;;===================================================================
;; keep emacs demon running for fast startups
;;===================================================================
(require 'server)
(if (not (server-running-p)) (server-start))

;;===================================================================
;; custom ediff
;;===================================================================
(defun ds-diffp (parent)
  (interactive "sParent path: ")
  (setq buffersplit (split-string buffer-file-name "/"))
  (setq relativepath "")
  (concat parent relativepath)
  (setq index 3)
  (while (< index (length buffersplit))
    (setq relativepath (concat relativepath "\\"))
    (setq relativepath (concat relativepath (elt buffersplit index)))    
    (setq index(1+ index))
    )
  (setq relativepath (concat parent relativepath))
  
  
  (ediff buffer-file-name relativepath))
;;    (ediff buffer-file-name )
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
                                 :background "blue"
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

;;END=========================================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lua-mode treemacs minimap diminish autopair swiper idle-highlight-mode expand-region auto-complete which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-even-diff-A ((t (:background "dim gray"))))
 '(ediff-even-diff-Ancestor ((t (:background "dim gray"))))
 '(ediff-even-diff-B ((t (:background "dim gray"))))
 '(ediff-even-diff-C ((t (:background "dim gray"))))
 '(ediff-odd-diff-A ((t (:background "dim gray"))))
 '(ediff-odd-diff-Ancestor ((t (:background "dim gray"))))
 '(ediff-odd-diff-B ((t (:background "dim gray"))))
 '(ediff-odd-diff-C ((t (:background "dim gray"))))
 '(minimap-active-region-background ((((background dark)) (:background "#3f4f57")) (t (:background "#C847D8FEFFFF"))) nil (quote minimap)))
 
