;; Copyright (c) 2020 Adam Massmann
;;
;; sml-basis-mode is released under the GPLv3 license.  See the file
;; COPYING for details.

;; Author: Adam Massmann <akm2203@columbia..edu>
;; URL: https://github.com/massma/sml-basis-mode
;; Version: 0.0.1
;; Keywords: mlton basis standard-ml
;; Package-Requires: ((sml-mode "6.10"))

;;; Commentary
;; See https://github.com/massma/sml-basis-mode &
;; http://mlton.org/ShowBasis for further information.

;;; TODO:
;; - [ ] I use global-auto-revert-mode so this is not an issue, but we
;;       could make a custom option to turn on/off revert mode locally
;;       in the *SML Basis* buffer. That way everytime we
;;       automatically recompile new basis files with MLton, we get
;;       access to the new information
;;
;; - [ ] Define a local keymap for the *SML-basis* buffer which binds
;;       functions for specific search types (signature, val,
;;       structure, exn, etc.)?
;;
;; - [ ] Implement a "goto thing at point" for the *SML-basis* buffer
;;       (by default this will only work on custom basis files, and
;;       when filenames within the basis file are relative to the
;;       directory containing the basis file). So, it actually might
;;       not be that useful. I'll wait and see if it is something I
;;       feel like is missing.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prelude
(require 'sml-mode)

(defconst sml-basis--dir
  (file-name-directory load-file-name))

(defconst sml-basis-file--standard
  (expand-file-name "default.basis" sml-basis--dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup sml-basis nil
  "A minor mode for searching MLton's basis files."
  :group 'sml)

(defcustom sml-basis-file sml-basis-file--standard
  "Optional action to perform when build fails."
  :type '(file)
  :group 'sml-basis
  :safe #'string-or-null-p)

(defconst sml-basis-buffer "*SML-basis*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search

(defun sml-basis-symdata-at-point ()
  "Get the sml identifier at point: cribbed from `sml-mode'."
  (save-excursion
    ;; move to end of current token only if we aren't already there
    (if (not (= (string-to-char " ") (char-after)))
        (sml-smie-forward-token))
    (let ((symname (sml-smie-backward-token)))
      (if (equal symname "op")
          (save-excursion (setq symname (sml-smie-forward-token))))
      (when (string-match "op " symname)
        (setq symname (substring symname (match-end 0)))
        (forward-word)
        (forward-comment (point-max)))
      symname)))

(defun sml-basis-generic-search (type)
  "Start a generic search without regexps, parameterized by TYPE.

TYPE is usually something like \"val\", \"signature\", etc."
  (isearch-mode t)
  (isearch-yank-string (concat "
" type " ")))

(defun sml-basis-generic-regexp-search (regexp)
  "Start a generic search with regexps, parameterized by REGEXP."
  (isearch-mode t t)
  ;; By default, isearch-yank-string wrapps regexp in regexp-quote,
  ;; which is not the behavior we want here, so use more "internal"
  ;; isearch functions.
  (isearch-process-search-string
   regexp (mapconcat 'isearch-text-char-description regexp "")))

(defconst sml-basis-identifier-regexp
  (rx "
" (or "val" "signature" "exn" "type" "con" "datatype") " "))

(defun sml-basis-regexp-search ()
  "Default regexp search for an identifier."
  (interactive)
  (sml-basis-generic-regexp-search
   sml-basis-identifier-regexp))

(defun sml-basis-val-search ()
  "Search for a val's identifier."
  (interactive)
  (sml-basis-generic-search "val"))

(defun sml-basis-sig-search ()
  "Search for a signature's identifier."
  (interactive)
  (sml-basis-generic-search "signature"))

(defun sml-basis-exception-search ()
  "Search for a exceptions's identifier."
  (interactive)
  (sml-basis-generic-search "exn"))

(defun sml-basis-struct-search ()
  "Search for a structures's identifier."
  (interactive)
  (sml-basis-generic-search "structure" ))

(defun sml-basis-type-search ()
  "Search for a type identifier."
  (interactive)
  (sml-basis-generic-search "type"))

(defun sml-basis-datatype-search ()
  "Search for a datatype identifier."
  (interactive)
  (sml-basis-generic-search "datatype"))

(defun sml-basis-type-constructor-search ()
  "Search for a type constructur identifier."
  (interactive)
  (sml-basis-generic-search "con"))

(defun sml-basis-maybe-switch-buffer ()
  "If we are not on the basis buffer, switch to it."
  (unless (string= (buffer-name (current-buffer))
                   sml-basis-buffer)
    (switch-to-buffer-other-window sml-basis-buffer))
  (goto-char (point-min)))

(defun sml-basis-search (sym)
  "Switch to basis buffer and search for any identifer.
SYM is the identifier to search for, and will default to the SML
symbol at point."
  (interactive
   (let ((default-sym (sml-basis-symdata-at-point)))
     (list
      (read-string
       (concat "Basis search term (default: " default-sym "): ")
       nil nil default-sym))))
  (sml-basis-maybe-switch-buffer)
  (sml-basis-val-search)
  (isearch-yank-string sym))

(defun sml-basis-type-signature-search ()
  "Search for a type signature."
  (interactive)
  (sml-basis-maybe-switch-buffer)
  (sml-basis-generic-regexp-search
   (concat sml-basis-identifier-regexp
           (rx (one-or-more (not (any ":" blank))) (or " : " ": ")))))

(defun sml-basis-switch-basis-buffer ()
  "Switch to the basis buffer."
  (interactive)
  (sml-basis-maybe-switch-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defvar sml-basis-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-h") 'sml-basis-switch-basis-buffer)
    (define-key map (kbd "C-c h") 'sml-basis-search)
    (define-key map (kbd "C-c t") 'sml-basis-type-signature-search)
    map)
  "The keymap associated with `sml-basis-mode'.")

(defvar sml-basis-in-basis-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") 'sml-)))

(define-minor-mode sml-basis-mode
  "Minor mode for searching MLton's basis files
"
  :lighter " basis"
  (if (not (member sml-basis-buffer (mapcar 'buffer-name (buffer-list))))
      (let ((buffer (current-buffer)))
        (find-file-read-only sml-basis-file)
        (rename-buffer sml-basis-buffer)
        (switch-to-buffer buffer))))

(provide 'sml-basis-mode)

;;; sml-basis-mode.el ends here
