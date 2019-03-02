;;; python-smart-execute.el --- Send blocks of code to a Python process

;; Copyright (C) 2015 Tom Bowles
;;
;; Created: 14 August 2015
;; Version: 0.1

;;; Commentary:

;; This package allows you to send lines, blocks or whole functions to
;; an inferior Python process.
;;
;; Suggested keybindings:
;;
;; (define-key python-mode-map (kbd "<f1>") #'python-smart-execute)
;; (define-key python-mode-map (kbd "<S-return>") #'python-smart-execute-next-line)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; code:
(require 'python)


(defun pse--start-indent-at-0 (string)
  "Given python code STRING, unindent it so the minimum indent is zero."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (forward-to-indentation 0)
    (when (> (current-column) 0)
      (indent-rigidly (point-min) (point-max) (- (current-column))))
    (buffer-substring (point-min) (point-max))))

(defun pse--shell-send-region (start end &rest unused)
  "Send the region delimited by START and END to inferior Python process.
Equivalent to `python-shell-send-region' but robustly handles
single line regions, and a regions including multiple
blocks (e.g. if block followed by else block).

`python-shell-send-region' has a number of issues when sending
single-line regions that are indented. We end up sending just
\"if True:\", which is a syntax error. This is fixed in Emacs 25,
as of commit 1fcc552ac27503c."
  (interactive "r")
  (python-shell-send-string
   (pse--start-indent-at-0
    (buffer-substring-no-properties start end))))

(defun pse--related-block-p ()
  "Return non-nil if the current block is a related block.
These are blocks that can only occur after another block, such as
an elif after an if."
  (save-excursion
    (python-nav-beginning-of-statement)
    (looking-at (rx symbol-start
                    (or "elif" "else" "except" "finally")
                    symbol-end))))

(defun pse--nav-end-of-related-blocks ()
  "Move to end of current block.
If there are related blocks, move to the end of them instead."
  (interactive "^")
  (when (python-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (python-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (python-nav-end-of-statement) t))
                      (pse--related-block-p)
                      (python-info-current-line-comment-p)
                      (python-info-current-line-empty-p))))
      (python-util-forward-comment -1)
      (point-marker))))

(defun pse--shell-send-block ()
  "Send the current block to the interpreter.
If the current block has relevant blocks afterwards (e.g. else,
except), then send that too."
  (interactive)
  (save-excursion
    (let (beg end)
      (python-nav-beginning-of-block)
      (beginning-of-line)
      (setq beg (point-marker))

      (pse--nav-end-of-related-blocks)
      (setq end (point-marker))

      (pse--shell-send-region beg end))))

(defun pse--statement-starts-defun-p ()
  "Return non-nil if current statement opens a defun."
  (save-excursion
    (python-nav-beginning-of-statement)
    (looking-at (python-rx defun))))

(defun pse--shell-send-statement ()
  "Send current Python statement to inferior Python process"
  (interactive)
  (save-excursion
    (let (beg end)
      (python-nav-beginning-of-statement)
      (setq beg (point))
      (python-nav-end-of-statement)
      (setq end (point))
      (pse--shell-send-region beg end))))

(defun pse--shell-send-dwim ()
  "Send the current function, block or line of code to the current Python process."
  ;; make sure we're at the end of the python shell
  (with-current-buffer (process-buffer (python-shell-get-process))
    (goto-char (point-max)))

  (cond
   ;; If the region is active, send that.
   ((region-active-p)
    (pse--shell-send-region (region-beginning) (region-end))
    (deactivate-mark))
   ;; If we're on a comment or empty line, don't send anything.
   ((or
     (python-info-current-line-comment-p)
     (python-info-current-line-empty-p)))
   ;; If we're on a the first line of a function, send the whole
   ;; function.
   ((pse--statement-starts-defun-p)
    (python-shell-send-defun nil)
    (python-nav-end-of-defun)
    (backward-char))
   ;; If we're on a block (for, while, if etc), send that whole block
   ;; and relevant blocks after (elif, finally etc).
   ((python-info-statement-starts-block-p)
    (pse--shell-send-block)
    (pse--nav-end-of-related-blocks))
   ;; Otherwise, send the current statement.
   (t
    (pse--shell-send-statement)
    (python-nav-end-of-statement))))

;;;###autoload
(defun python-smart-execute ()
  "Send the Python code at point to the current Python process.
This may be a line, a block of code, or a whole function.
If the region is active, send that instead.

See also `python-smart-execute-next-line'.  and
`python-smart-execute-no-move'."
  (interactive)
  (unless (python-shell-get-process)
    (user-error "You need a Python session to send code to, try M-x run-python"))
  (pse--shell-send-dwim)
  (python-nav-forward-statement))

;;;###autoload
(defun python-smart-execute-next-line ()
  "Move to the next line, even if blank, after executing.
Handy for incrementally developing functions."
  (interactive)
  (pse--shell-send-dwim)
  (forward-line)
  (python-nav-beginning-of-statement))

;;;###autoload
(defun python-smart-execute-no-move ()
  "Execute, but don't move point."
  (interactive)
  (pse--shell-send-dwim))

(provide 'python-smart-execute)
;;; python-smart-execute.el ends here
