;; bind both f1 and shift-return to the two different versions
(define-key python-mode-map (kbd "<f1>") 'python-smart-execute)
(define-key python-mode-map [(shift return)] 'python-smart-execute-next-line)

(define-key python-mode-map (kbd "<f12>") 'python-erase-shell)
(define-key python-mode-map (kbd "<tab>") 'python-complete-or-indent)
(define-key python-mode-map (kbd "<f5>") 'python-new-plot)
(define-key python-mode-map (kbd "S-<f5>") 'python-close-current-plot)
(define-key python-mode-map (kbd "C-S-<f5>") 'python-close-all-plots)
(define-key python-mode-map (kbd "<f3>") 'python-lookup-current-symbol-help)
(define-key python-mode-map (kbd "S-<f3>") 'python-grep-current-symbol-as-tag)
(define-key python-mode-map (kbd "<f4>") 'python-find-in-lab)
(define-key python-mode-map (kbd "<f2>") 'python-print-current-symbol)
(define-key python-mode-map (kbd "S-<f2>") 'python-plot-current-symbol)
;(define-key python-mode-map (kbd "<f3>") 'python-lookup-current-symbol-as-tag)

(define-key inferior-python-mode-map (kbd "<f5>") 'python-new-plot)
(define-key inferior-python-mode-map (kbd "S-<f5>") 'python-close-current-plot)
(define-key inferior-python-mode-map (kbd "C-S-<f5>") 'python-close-all-plots)
(define-key inferior-python-mode-map (kbd "<f12>") 'python-erase-shell)



(defun py ()
  (interactive)
  (save-window-excursion
    (command-execute 'run-python)
  )
  (switch-to-buffer "*Python*")
)


;; define matplotlib figure utility functions
(defun python-new-plot ()
  (interactive)
  (python-shell-send-string "figure()")
)

(defun python-close-current-plot ()
  (interactive)
  (python-shell-send-string "close()")
)

(defun python-close-all-plots ()
  (interactive)
  (python-shell-send-string "close('all')")
)

;;; print symbol at point
(defun python-print-current-symbol ()
  (interactive)
  (python-shell-send-string (python-info-current-symbol))
)

;;; plot symbol at point
(defun python-plot-current-symbol ()
  (interactive)
  (python-shell-send-string (concat "figure(); plot(" (python-info-current-symbol) ")" ))
)

;;; look up tag for symbol
(defun python-lookup-current-symbol-as-tag ()
  (interactive)
  (xref-find-apropos (python-info-current-symbol))
)

;;; look up help for symbol
(defun python-lookup-current-symbol-help ()
  (interactive)
  (python-erase-shell)
  (python-shell-send-string (concat (python-info-current-symbol) "?" ))
  ;;(beginning-of-buffer)
)

(defun python-grep-current-symbol-as-tag ()
  (interactive)
  (xref-find-apropos (python-info-current-symbol))
)
;;; find in ahl.lab shortcut
(defun python-find-in-lab ()
  (interactive)
  (python-shell-send-string (concat "find_in_lab('" (python-info-current-symbol) "')" ))
)


(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun python-erase-shell ()
  (interactive)
  (save-window-excursion
    (python-shell-switch-to-shell)
    (comint-clear-buffer)
    (comint-send-input)
    )
  )



;;; context aware tab
;;; Do completion when at end of line, indentation otherwise
;; (require 'company)
(defun python-complete-or-indent ()
  (interactive)
  (if (and (python-info-end-of-statement-p) (not (python-info-current-line-comment-p)) (not (python-info-current-line-empty-p)))
      (progn
        (completion-at-point)
        ;; (company-complete)
        )
    (indent-for-tab-command)
    )
  )

(provide 'python-extensions)
;;; python-extensions.el ends here
