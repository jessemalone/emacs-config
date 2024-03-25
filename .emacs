(require 'package)

(defun install-packages (&rest packages)
  (dolist (package packages)
    (unless (package-installed-p package)
        (progn
         (package-refresh-contents)
         (package-install package)
         )
      ) 
    (require package)
    )
  )
 
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-mode t)
 '(custom-enabled-themes '(wombat))
 '(evil-collection-setup-minibuffer t)
 '(evil-mode t)
 '(evil-want-keybinding nil)
 '(global-evil-collection-unimpaired-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(irony vterm lsp-clanmgd edit-indirect lsp-pyright use-package yasnippet go-mode which-key haskell-mode lsp-mode company-terraform dockerfile-mode yaml-mode ivy evil-collection evil js2-mode tide company-ctags counsel-etags company helm))
 '(tool-bar-mode nil)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#27c127c127c1"))))
 '(company-scrollbar-fg ((t (:background "#25f225f225f2"))))
 '(company-tooltip ((t (:inherit default :background "#24dd24dd24dd"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

(setq while-no-input-ignore-events 'nil)



(install-packages
 'use-package
 'js2-mode
 'helm
 'which-key
 'evil
 'evil-collection
 'ivy
 'company
 'company-irony
 ;; 'company-ctags
 ;; 'company-terraform
 'tide
 'yaml-mode
 'dockerfile-mode
 'haskell-mode
 'lsp-mode
 'lsp-pyright
; 'lsp-haskell
 'go-mode
 'yasnippet
 'irony
 'projectile
 'rtags
 'anaconda-mode
 )

;; Helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; Evil
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(evil-mode 1)
(evil-collection-init)

;; Completion
;; (use-package counsel-etags
;;   :ensure t
;;   :bind (("C-\\" . (lambda () (interactive) (split-window-below) (counsel-etags-find-tag-at-point))))
;;   :init
;;   (add-hook 'prog-mode-hook
;;         (lambda ()
;;           (add-hook 'after-save-hook
;;             'counsel-etags-virtual-update-tags 'append 'local)))
;;   :config
;;   (setq counsel-etags-update-interval 60)
;;   (push "build" counsel-etags-ignore-directories))

;; (with-eval-after-load 'company
;;   (company-ctags-auto-setup))

;; Company colours  (require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Company always on
(add-hook 'after-init-hook 'global-company-mode)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-<tab>") 'company-complete))
(add-to-list 'company-backends '(company-files))

;; Tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))


(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'javascript-mode-hook #'setup-tide-mode)
(add-hook 'js-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)

(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; Dockerfile
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(setq dockerfile-mode-command "docker")

(add-hook 'yaml-mode-hook
'(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;(define-key minibuffer-local-map (kbd "s-j") 'next-history-element)
;;(define-key minibuffer-local-map (kbd "s-k") 'previous-history-element)

;; Vterm
(use-package vterm
  :ensure t)
(use-package multi-vterm
  :ensure t)

;; Easy Terminal
(global-set-key (kbd "C-c '") '(lambda () (interactive) (multi-vterm)))
(setq term-suppress-hard-newline t)

;; Bell
(setq visible-bell t)

;; Desktop Save
(desktop-save-mode 1)
(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
 (lambda ()
   (frameset-restore
    desktop-saved-frameset
    :reuse-frames (eq desktop-restore-reuses-frames t)
    :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
    :force-display desktop-restore-in-current-display
    :force-onscreen desktop-restore-forces-onscreen)))

; Tabs vs. Spaces (be nice) 
; From https://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(setq-default indent-tabs-mode nil)
(infer-indentation-style)

;; Misc options

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; line numbers in all programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Winner mode
(winner-mode 1)

;; Projectile
(require 'projectile)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)


;; LSP Modes
;; =================================

;; Python LSP
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

;; Javascript/Typescript
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'javascript-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'js2-mode-hook #'lsp-deferred)

;; Haskell
;(add-hook 'haskell-mode-hook #'lsp-deferred)
;(add-hook 'haskell-literate-mode-hook #'lsp-deferred)

;; Gopls
(add-hook 'go-mode-hook #'lsp-deferred)

;; C++
(add-hook 'c++-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'company-mode)
  (add-hook 'c++-mode-hook #'yas-minor-mode)

;; C/C++ Irony Mode
(defun cxx-hook ()
  'irony-mode

  (setq-local company-backends
              '((company-capf :with company-yasnippet))))

(add-hook 'c++-mode-hook 'cxx-hook)

(add-hook 'c-mode-hook 'cxx-hook)
(add-hook 'objc-mode-hook 'cxx-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Gecko project type for Projectile - Uncomment for work on the firefox codebase
;;(projectile-register-project-type 'gecko
;;                                  '("mach" "moz.build")
;;                                  "python mach --log-no-times build"
;;                                  "python mach mochitest"
;;                                  "python mach run")

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


