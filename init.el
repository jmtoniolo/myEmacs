;; Emacs init file 2026.08.07 JMT

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
(setq tramp-verbose 10) 

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
;;minimap toggle
(global-set-key (kbd "<f12>") 'minimap-mode)
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

;;minimap on right side
(use-package minimap
  :ensure t
  :config
          (setq minimap-recenter-type 'middle)
          (setq minimap-window-location 'right))

;;tramp for remote editing
(use-package tramp
  :ensure t
  :config
  (setq default-tramp-method "plink")
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("78b4f4cf98bd74bb4efe1da37e9ddd8b72db24dcd7933685aff4a313f03428a5" default))
 '(ediff-diff-options "--binary -w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(electric-indent-mode nil)
 '(grep-command
   "grep --exclude-dir={Isrc} --color=always -s -n -e 2>/dev/null")
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.bak" "*.obj" "*.map" "*.ico" "*.pif" "*.lnk" "*.a" "*.ln" "*.blg" "*.bbl" "*.dll" "*.drv" "*.vxd" "*.386" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo"))
 '(package-selected-packages
   '(vlf magit git matlab-mode async highlight-doxygen cmake-mode csharp-mode markdown-mode+ markdown-mode md-readme lua-mode treemacs minimap diminish swiper idle-highlight-mode expand-region auto-complete which-key use-package))
 '(standard-indent 2)
 '(vc-git-program "/home/gitdet/bin/git")
 '(warning-suppress-types '((mule)))
 '(whitespace-line-column 90))
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
 '(minimap-active-region-background ((((background dark)) (:background "#3f4f57")) (t (:background "#C847D8FEFFFF"))) nil 'minimap)
 '(which-func ((t (:foreground "alice blue"))))
 '(whitespace-empty ((t (:foreground "red" :strike-through t))))
 '(whitespace-line ((t (:background "dark slate blue"))))
 '(whitespace-newline ((t (:foreground "dark slate gray" :weight normal))))
 '(whitespace-space ((t (:distant-foreground "dark slate gray" :foreground "dark slate gray"))))
 '(whitespace-trailing ((t (:foreground "red" :strike-through t :weight bold)))))
 ;;original was red... what a harsh colour
 
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)

(find-file "c:/Users/z004ka2x/OneDrive - Siemens AG/Documents/personal/Project_Notes.org")
