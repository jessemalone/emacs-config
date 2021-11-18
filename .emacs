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
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-mode t)
 '(custom-enabled-themes '(wheatgrass))
 '(evil-collection-setup-minibuffer t)
 '(evil-mode t)
 '(evil-want-keybinding nil)
 '(global-evil-collection-unimpaired-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(which-key lsp-haskell haskell-mode lsp-python-ms lsp-mode dockerfile-mode yaml-mode ivy evil-collection evil js2-mode tide company-ctags counsel-etags company helm))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
 'company-ctags
 'tide
 'yaml-mode
 'dockerfile-mode
 'haskell-mode
 'lsp-mode
 'lsp-python-ms
 'lsp-haskell
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
(use-package counsel-etags
  :ensure t
  :bind (("C-\\" . (lambda () (interactive) (split-window-below) (counsel-etags-find-tag-at-point))))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))

(with-eval-after-load 'company
  (company-ctags-auto-setup))


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

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

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

;; Easy Terminal
(global-set-key (kbd "C-c '") '(lambda () (interactive) (ansi-term "/bin/bash")))
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
(global-display-line-numbers-mode 1)

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


;; LSP Modes
;; =================================

;; Python LSP
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))  ; or lsp-deferred

;; Javascript/Typescript
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'javascript-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'js2-mode-hook #'lsp-deferred)

;; Haskell
(add-hook 'haskell-mode-hook #'lsp-deferred)
(add-hook 'haskell-literate-mode-hook #'lsp-deferred)
