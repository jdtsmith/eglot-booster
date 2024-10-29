;;; eglot-booster.el --- Boost eglot using lsp-booster -*- lexical-binding: t; -*-
;; Copyright (C) 2024  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/eglot-booster
;; Package-Requires: ((emacs "29.1") (jsonrpc "1.0") (eglot "1.0") (seq "2.24"))
;; Version: 0.1.0
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
(require 'eglot)
(require 'jsonrpc)

(defcustom eglot-booster-no-remote-boost nil
  "If non-nil, do not boost remote hosts."
  :group 'eglot
  :type 'boolean)

(defcustom eglot-booster-io-only nil
  "If non-nil, do not translate JSON into bytecode.
I/O buffering is still performed."
  :group 'eglot
  :type 'boolean)

(defun eglot-booster-plain-command (com)
  "Test if command COM is a plain eglot server command."
  (and (consp com)
       (not (integerp (cadr com)))
       (not (keywordp (car com)))
       (not (memq :autoport com))))

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
	     (buf (process-buffer proc)))
    (unless (and (file-remote-p default-directory) eglot-booster-no-remote-boost)
      (if (file-remote-p default-directory) ; com will likely be /bin/sh -i or so
	  (when (seq-find (apply-partially #'string-search "emacs-lsp-booster")
			  (process-get proc 'remote-command)) ; tramp applies this
	    (with-current-buffer buf (setq eglot-booster-boosted t)))
	(when (string-search "emacs-lsp-booster" (car-safe com))
	  (with-current-buffer buf (setq eglot-booster-boosted t)))))))

(defvar eglot-booster--boost
  '("emacs-lsp-booster" "--json-false-value" ":json-false" "--"))
(defvar eglot-booster--boost-io-only
  '("emacs-lsp-booster" "--disable-bytecode" "--"))

(defun eglot-booster--wrap-contact (args)
  "Wrap contact within ARGS if possible."
  (let ((contact (nth 3 args))
	(def-args (if eglot-booster-io-only
		      eglot-booster--boost-io-only
		    eglot-booster--boost)))
    (cond
     ((and eglot-booster-no-remote-boost (file-remote-p default-directory)))
     ((functionp contact)
      (setf (nth 3 args)
	    (lambda (&optional interactive)
	      (let ((res (funcall contact interactive)))
		(if (eglot-booster-plain-command res)
		    (append def-args res)
		  res)))))
     ((eglot-booster-plain-command contact)
      (setf (nth 3 args) (append def-args contact))))
    args))

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
    (unless eglot-booster-io-only
      (advice-add 'jsonrpc--json-read :around #'eglot-booster--jsonrpc--json-read))
    (advice-add 'eglot--connect :filter-args #'eglot-booster--wrap-contact)
    (add-hook 'eglot-server-initialized-hook #'eglot-booster--init))
   (t
    (advice-remove 'jsonrpc--json-read #'eglot-booster--jsonrpc--json-read)
    (advice-remove 'eglot--connect #'eglot-booster--wrap-contact)
    (remove-hook 'eglot-server-initialized-hook #'eglot-booster--init))))

(provide 'eglot-booster)
;;; eglot-booster.el ends here
