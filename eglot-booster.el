;;; eglot-booster.el --- Boost eglot using lsp-booster -*- lexical-binding: t; -*-
;; Copyright (C) 2024  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/eglot-booster
;; Package-Requires: ((emacs "29.1") jsonrpc eglot seq)
;; Version: 0.0.1
;; Keywords: convenience, programming
;; Prefix: eglot-booster
;; Separator: -

;; eglot-booster is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; eglot-booster is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This small minor mode boosts eglot with emacs-lsp-booster.
;; Using it is simple:
;; 
;; 1. Download/build a recent emacs-lsp-booster from
;;    https://github.com/blahgeek/emacs-lsp-booster using the
;;    instructions there.
;; 2. Enable eglot-booster-mode either in your init or, interactively,
;;    use M-x eglot-booster-mode.
;; 3. Use eglot like normal.
;;
;; You can disable boosting by turning the minor mode off; any boosted
;; eglot servers will need to be restarted.  Note: boosting works only
;; with local lsp servers programs which communicate via standard
;; input/output, not remote or network-port LSP servers.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'seq)
(require 'eglot)
(require 'jsonrpc)

(defun eglot-booster-plain-command (com)
  "Test if command COM is a plain eglot server command."
  (and (consp com)
       (not (integerp (cadr com)))
       (not (seq-intersection '(:initializationOptions :autoport) com))))

(defvar-local eglot-booster-boosted nil)
(defun eglot-booster--jsonrpc--json-read (orig-func)
  "Read JSON or bytecode, wrapping the ORIG-FUNC JSON reader."
  (if eglot-booster-boosted ; local to process-buffer
      (or (and (= (following-char) ?#)
	       (let ((bytecode (read (current-buffer))))
		 (when (byte-code-function-p bytecode)
		   (funcall bytecode))))
	  (funcall orig-func))
    ;; Not in a boosted process, fallback
    (funcall orig-func)))

(defun eglot-booster--init (server)
  "Register eglot SERVER as boosted if it is."
  (when-let ((server)
	     (proc (jsonrpc--process server))
	     (com (process-command proc))
	     ((string-search "emacs-lsp-booster" (car-safe com)))
	     (buf (process-buffer proc)))
    (setf (buffer-local-value 'eglot-booster-boosted buf) t)))

(defun eglot-booster--wrap (&optional unwrap)
  "Wrap relevant entries in `eglot-server-programs'.
If UNWRAP is non-nil, remove the wrapping."
  (let ((cnt 0)
	(boost '("emacs-lsp-booster" "--json-false-value" ":json-false" "--")))
    (dolist (entry eglot-server-programs)
      (cond
       ((functionp (cdr entry))
	(if unwrap			; restore old function
	    (when-let ((old-fun
			(condition-case nil
			    (funcall (cdr entry) nil 'old-func)
			  (wrong-number-of-arguments nil))))
	      (cl-incf cnt)
	      (setcdr entry old-fun))
	  (cl-incf cnt)
	  (let ((this-fun (cdr entry)))
	    (setcdr entry (lambda (interactive &optional return-old-func)
			    (if return-old-func this-fun
			      (let ((res (funcall this-fun interactive)))
				(if (eglot-booster-plain-command res)
				    (append boost res)
				  res))))))))
       ((eglot-booster-plain-command (cdr entry))
	(if unwrap
	    (when (string= (cadr entry) (car boost))
	      (cl-incf cnt)
	      (setcdr entry (seq-subseq (cdr entry) (length boost))))
	  (setcdr entry (append boost (cdr entry)))
	  (cl-incf cnt)))))
    (message "%s %d eglot-server-programs%s"
	     (if unwrap "Removed boost from" "Boosted") cnt
	     (if unwrap " (restart eglot processes to take effect)" ""))))

;;;###autoload
(define-minor-mode eglot-booster-mode
  "Minor mode which boosts plain eglot server programs with emacs-lsp-booster.
The emacs-lsp-booster program must be compiled and available on
variable `exec-path'.  Only local stdin/out-based lsp servers can
be boosted."
  :global t
  :group 'eglot
  (cond
   (eglot-booster-mode
    (unless (executable-find "emacs-lsp-booster")
      (setq eglot-booster-mode nil)
      (user-error "The emacs-lsp-booster program is not installed"))
    (advice-add 'jsonrpc--json-read :around #'eglot-booster--jsonrpc--json-read)
    (add-hook 'eglot-server-initialized-hook #'eglot-booster--init)
    (unless (get 'eglot-booster-mode 'boosted)
      (eglot-booster--wrap)
      (put 'eglot-booster-mode 'boosted t)))
   (t (advice-remove 'jsonrpc--json-read #'eglot-booster--jsonrpc--json-read)
      (remove-hook 'eglot-server-initialized-hook #'eglot-booster--init)
      (eglot-booster--wrap 'unwrap)
      (put 'eglot-booster-mode 'boosted nil))))

(provide 'eglot-booster)
;;; eglot-booster.el ends here
