;;; Code:

;;;;;;;;;;;;;;;;
;; Emacs core ;;
;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-M-h") 'backward-kill-word)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Effectively disables customize
(setq custom-file (make-temp-file "emacs-custom"))


;; See if garbage collection is causing a slowdown
;; by removing it in minibuffers
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; New auto-save stuff for Emacs 26.1
(auto-save-mode 0)
(auto-save-visited-mode t)

(setq
   backup-directory-alist '(("." . "~/.emacs.d/backup/"))
   backup-by-copying t    ; Don't delink hardlinks
   version-control t      ; Use version numbers on backups
   delete-old-versions t  ; Automatically delete excess backups
   kept-new-versions 6    ; how many of the newest versions to keep
   kept-old-versions 2    ; and how many of the old
   )

;; whitespace mode
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs empty trailing lines-tail))

(add-hook 'text-mode-hook (lambda() (whitespace-mode t)))
(add-hook 'prog-mode-hook (lambda() (whitespace-mode t)))

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'todo-mode
;;        (lambda () (remove-hook 'before-save-hook 'delete-trailing-whitespace)))


;; disable startup message
(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)

(electric-indent-mode t)
;; (global-hl-line-mode t) ;; Maybe turn this off for comint mode?

(mapc (lambda (mode-hook)
        (add-hook mode-hook #'auto-fill-mode)
        (add-hook mode-hook #'hl-line-mode))
      '(text-mode-hook
        org-mode-hook))

(add-hook 'prog-mode-hook 'hl-line-mode)

(setq create-lockfiles -1)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))


;; Ignore duplicates in input history for comint mode
(setq comint-input-ignoredups t)
;; If you press enter and the pointer is far up in comint mode, it would by default
;; send all the outputs to the interpreter as new inputs.
;; Instead, we move to the end-of-buffer first
(setq comint-get-old-input (lambda () (end-of-buffer) (comint-get-old-input-default)))

;; There's a weird thing with Inferior Python mode (IPython/Jupyter?) that sometimes makes
;; the input text read-only, and this function can make the prompt writable again.
(defun set-region-writeable (begin end)
  "Removes the read-only text property from the marked region.

Use `set-region-read-only' to set this property."
  ;; See https://stackoverflow.com/questions/7410125
  (interactive "r")
  (with-silent-modifications
    (remove-text-properties begin end '(read-only t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up proxy information ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (file-exists-p "~/.emacs.d/ahlproxy.el")
  (load-file "~/.emacs.d/ahlproxy.el")
  (proxy-on)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up packages using use-package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;
;; Setup packages ;;
;;;;;;;;;;;;;;;;;;;;

;; useful comments here https://jamiecollinson.com/blog/my-emacs-config/

(use-package diminish :ensure t)

(use-package zenburn-theme :ensure t)

;; TODO: Is this one actually needed?
(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

(use-package ido-completing-read+
  :ensure t
  :after ido
  :config
  (ido-everywhere t)
  (ido-ubiquitous-mode t))

(use-package flx-ido
  :ensure t
  :after ido)

(use-package ido
  :ensure t
  :custom
  (ido-enable-prefix nil)
  (ido-enable-flex-matching t)
  (ido-use-filename-at-point 'guess)
  (ido-max-prospects 10)
  ;; (ido-save-directory-list-file (expand-file-name "ido.hist" prelude-savefile-dir))
  (ido-default-file-method 'selected-window)
  (ido-use-faces nil) ;; disable ido faces to see flx highlights
  :init
  (ido-mode t)
  :config
  (flx-ido-mode t)  ;; smarter fuzzy matching for ido
  )

(use-package smex
  :ensure t
  :bind
  ("M-x" . smex)
  ("C-x C-m" . smex)
  ("M-X" . smex-major-mode-commands)
  :config
  (smex-initialize)
  )


;; (use-package flx
;;   :ensure t
;;   :after ivy)

;; (use-package ivy
;;   :ensure t
;;   :diminish ivy-mode
;;   :bind
;;   (("C-c C-r" . ivy-resume)
;;    ("C-x B" . ivy-switch-buffer-other-window))
;;   :custom
;;   (ivy-use-virtual-buffers t)
;;   ;; enable-recursive-minibuffers t
;;   (ivy-initial-inputs-alist nil)
;;   (ivy-re-builders-alist
;;    '((read-file-name-internal . ivy--regex-fuzzy)
;;      (ivy-switch-buffer . ivy--regex-fuzzy)
;;      (counsel-M-x . ivy--regex-fuzzy)
;;      (t . ivy--regex-plus)))
;;   :config
;;   (ivy-mode))

;; (use-package counsel
;;   :ensure t
;;   :after ivy
;;   :diminish t
;;   :init
;;     (use-package smex :ensure t)
;;   :bind*
;;     (;("M-x" . counsel-M-x)
;;      ("C-x C-m" . counsel-M-x)
;;      ;("C-x C-f" . counsel-find-file)
;;      )
;;   :config
;;     (counsel-mode))

;; (use-package swiper
;;   :ensure t
;;   :after ivy
;;   :bind (("C-s" . swiper)))

;; (use-package counsel-projectile
;;   :ensure t
;;   :after projectile
;;   :diminish t
;;   :config
;;   (add-hook 'after-init-hook 'counsel-projectile-mode))

(use-package projectile
  :ensure t
  ;; :after ivy
  ;; :after ido
  :config
  ;(setq projectile-completion-system 'ivy)
  (projectile-mode t)
  )

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (add-hook 'after-init-hook 'which-key-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :init
  (setq
     aw-scope 'frame
     aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(use-package expand-region
    :ensure t
    :bind ("C-=" . er/expand-region))

(use-package hl-todo
  :ensure t
  :config
    (global-hl-todo-mode t))

(defun riseth-text-mode-defaults ()
  "Default hooks for `text-mode'."
  (turn-on-auto-fill)
  (smartparens-mode +1)
  (writegood-mode +1))

(setq riseth-text-mode-hook 'riseth-text-mode-defaults)
(add-hook 'text-mode-hook (lambda ()
                            (run-hooks 'riseth-text-mode-hook)))

(use-package writegood-mode
  :ensure t
  :commands writegood-mode
  :diminish writegood-mode
  :init (mapc (lambda (mode-hook)
                (add-hook mode-hook #'writegood-mode))
              '(text-mode-hook
                org-mode-hook)))

(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
    (add-hook 'markdown-mode-hook 'turn-on-smartparens-mode))
  :config
  (progn
    (show-smartparens-global-mode t)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package company
  :ensure t
  :commands (company-mode)
  :init
  (global-company-mode t)
  :custom
  ;; Taken from prelude
  (company-idle-delay 0.5)
  (company-show-numbers t)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  ;; invert the navigation direction if the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t)
  :config (progn
  ;;           (add-hook 'prog-mode-hook 'company-mode)
            (bind-key "C-n" #'company-select-next company-active-map)
            (bind-key "C-p" #'company-select-previous company-active-map))
  )

(use-package flycheck
  :ensure t
  :diminish " ✓"
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
    TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
    TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
 ;; to have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (rainbow-delimiters-mode)
              (company-mode)
              (smartparens-mode)
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)
              (setq TeX-PDF-mode t)
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t))))


(use-package reftex
  :ensure t
  :defer t
  :init
  (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

;;; Can also add a package called ivy-bibtex

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (setq mouse-wheel-follow-mouse t
        pdf-view-resize-factor 1.10
        pdf-view-display-size 'fit-page
        pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  )

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :defer t
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package highlight-indent-guides
  :diminish
  :ensure t)

;; ;; Debugger
;; (use-package realgud
;;   :ensure t)

(use-package python
  :ensure t
  :mode ("\\.py" . python-mode)
  :init
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'inferior-python-mode-hook 'turn-on-smartparens-mode)
  (require 'smartparens-python)
  :custom
  (python-shell-interpreter "jupyter")
  (python-shell-interpreter-args "console --simple-prompt")
  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (add-to-list 'load-path "~/.emacs.d/python/")
  (require 'python-smart-execute)
  (require 'python-extensions)
  (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)
  (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-matching-input-from-input)
  )


;; (use-package python
;;   :ensure t
;;   :mode ("\\.py" . python-mode)
;;   :init
;;   (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
;;   (add-hook 'python-mode-hook 'turn-on-smartparens-mode)
;;   (add-hook 'inferior-python-mode 'turn-on-smartparens-mode)
;;   (require 'smartparens-python)
;;   ;;(python-shell-font-lock-turn-off)
;;   (use-package elpy
;;     :ensure t
;;     :commands elpy-enable
;;     :config
;;     (setq ;; elpy-rpc-python-command "python2"
;;        ;; elpy-modules (dolist (elem '(elpy-module-highlight-indentation
;;        ;;                           elpy-module-yasnippet))
;;        ;;             (remove elem elpy-modules))
;;        ;python-shell-interpreter "ipython"
;;        ;python-shell-interpreter-args "-i --simple-prompt"
;;           python-shell-interpreter "jupyter"
;;        python-shell-interpreter-args "console --simple-prompt"
;;        python-shell-prompt-detect-failure-warning nil
;;        elpy-company-add-completion-from-shell t
;;        elpy-shell-use-project-root nil
;;        ))
;;     (add-to-list 'python-shell-completion-native-disabled-interpreters
;;               "jupyter")

;;     (elpy-enable)

;;     (add-to-list 'load-path "~/.emacs.d/python/")
;;     (require 'python-smart-execute)
;;     (require 'python-extensions)

;;   (define-key inferior-python-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)
;;   (define-key inferior-python-mode-map (kbd "<down>") 'comint-next-matching-input-from-input))


;; ;; Emacs slows down a lot whenever Inferior Python hits an error and prints the trace
;; ;; I think it may be due to font-lock, so let's see if this helps
;; ;; See https://github.com/jorgenschaefer/elpy/wiki/Customizations#enable-full-font-locking-of-inputs-in-the-python-shell
;; (advice-add 'elpy-shell--insert-and-font-lock
;;             :around (lambda (f string face &optional no-font-lock)
;;                       (if (not (eq face 'comint-highlight-input))
;;                           (funcall f string face no-font-lock)
;;                         (funcall f string face t)
;;                         (python-shell-font-lock-post-command-hook))))

;; (advice-add 'comint-send-input
;;             :around (lambda (f &rest args)
;;                       (if (eq major-mode 'inferior-python-mode)
;;                           (cl-letf ((g (symbol-function 'add-text-properties))
;;                                     ((symbol-function 'add-text-properties)
;;                                      (lambda (start end properties &optional object)
;;                                        (unless (eq (nth 3 properties) 'comint-highlight-input)
;;                                          (funcall g start end properties object)))))
;;                             (apply f args))
;;                         (apply f args))))


(provide 'init)
;;; init.el ends here
