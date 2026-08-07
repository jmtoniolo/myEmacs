;; Emacs init file 2026.08.07 JMT

;;===================================================================
;; package managers
;;===================================================================
(require 'package)

(setq package-enable-at-startup nil)
;; elpa.gnu.org is blocked on this network (TCP connects but HTTPS hangs),
;; so we rely on MELPA only, which mirrors virtually everything we need.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;garbage collection setting
(setq gc-cons-threshold 64000000) 
(setq tramp-verbose 1) 

;;===================================================================
;; tweaks and minor settings
;;===================================================================
;;font
(set-face-attribute 'default nil :height 105)
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
(set-face-background 'hl-line "#404040")
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
;; Don't ring the bell
(setq ring-bell-function 'ignore)
;;y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; Disable the horrid auto-save
(setq auto-save-default nil)

;; Show whitespace
(global-set-key (kbd "<f10>") 'whitespace-mode)
;;copy file path of current buffer to clip board
(global-set-key (kbd "C-c C-/") 'er-copy-file-name-to-clipboard)
;; Easy undo key
(global-set-key (kbd "C-/") 'undo)
;; Comment or uncomment the region
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;;exand region
(global-set-key (kbd "M-m") 'er/expand-region)

;;mouse keys
(global-set-key (kbd "<mouse-4>") 'next-buffer)
(global-set-key (kbd "<mouse-5>") 'previous-buffer)
;;use side scroller to scroll horizontally
(setq mouse-wheel-tilt-scroll 1)

;;keep buffer up to date if file changes outside emacs
(global-auto-revert-mode t)
;;title bar shows full path
(setq-default frame-title-format '("%b"))

;; Smooth out the scrolling
 (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; # line at a time
 (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

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
  :ensure t)

;; autopair
(electric-pair-mode 1)

;;ivy for help with M-x commands
(use-package ivy
  :ensure t
  :config (ivy-mode t))

;;tramp for remote editing
(use-package tramp
  :config
  (setq tramp-default-method "plink")
  (customize-set-variable 'tramp-default-user "johton2u"))
;;(setenv "PATH" (concat "c:/Users/john.toniolo/Documents/putty/;" (getenv "PATH")))

;;===================================================================
;; Custom Functions
;;===================================================================
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
;; custom ediff
;;===================================================================
(defun ds-diff (parent removepath)  
  (setq buffersplit (split-string buffer-file-name "/"))
  (setq relativepath "")
  (concat parent relativepath)
  (setq index removepath) ;;//<remote machine>/HOME/WRKSPS/<level>/<workspace>/ must be removed and replaced with //nas01deu/BSF/<level>
                          ;;//1               /2   /3     /4      /5          /7
  (while (< index (length buffersplit))
    (setq relativepath (concat relativepath "/"))
    (setq relativepath (concat relativepath (elt buffersplit index)))    
    (setq index(1+ index))
    )
  (setq relativepath (concat parent relativepath))
  
  
  (ediff buffer-file-name relativepath))

(defun ds-ndiff (parent) ;;diff from network
  (interactive "sParent path: ") ;;get parent path from user
  (ds-diff parent 7))

(defun ds-ldiff (parent) ;;from from local machine
  (interactive "sParent path: ") ;;get parent path from user
  (ds-diff parent 6))



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
  (find-file "/plink:orw-johton2u-r8.wv.mentorg.com:/wv/johton2u") )

(defun lmweb-connect ()
  (interactive)
  (find-file "/plink:orw-lmweb-r8.wv.mentorg.com:/wv/johton2u") )

;; (defun icbuild-connect ()
;;   (interactive)
;;   (find-file "/plink:icbuild@orw-johton2u-r8.wv.mentorg.com:/wv/callic") )

(defun icbuild-connect ()
  (interactive)
  (find-file "/plink:icbuild@icbuild-login:/wv/callic") )

(setq ediff-diff-program "C:\\Program Files\\Git\\usr\\bin\\diff.exe")
(setq vc-handled-backends nil) 
;; END=========================================================================================
(let ((notes-file "c:/Users/z004ka2x/OneDrive - Siemens AG/Documents/personal/Project_Notes.org"))
  (when (file-exists-p notes-file)
    (find-file notes-file)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
