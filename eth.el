;;; eth.el --- Emacs translation helpers -*- lexical-binding: t -*-

;; Copyright (C) 2021
;; SPDX-License-Identifier: CECILL-2.1
;; Credits:
;; - Parts of the code are taken and modified from various contributors
;;   of the GNU Emacs mailing list.
;; - The `eth-edit-in-buffer' function has been modified from:
;;   https://www.reddit.com/r/emacs/comments/cajk6h/readfrombuffer/

;; Author: Frédéric Santos
;; Version: 1.0.0
;; URL: https://github.com/frederic-santos/eth
;; Package-Requires: ((emacs "24.3") (google-translate "0.12.0"))

;; This file is *not* part of GNU Emacs.

;;; Commentary:
;; This package provides very basic helpers to translate sentences
;; in place in an Emacs buffer.

;;; Code:
(require 'google-translate)
(require 'google-translate-smooth-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHLIGHTING FEATURES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar eth-sentence-overlay nil
  "Contains overlay information for the current buffer.
If nil, there is no active overlay.")

(defface eth-highlight-face
  '((t :foreground "white"
       :background "#0087ff"
       :weight bold))
  "Custom face for highlighted segments.")

(defun eth-remove-highlight ()
  "Remove highlight of a currently highlighted sentence."
  (interactive)
  (when (overlayp eth-sentence-overlay)
    (delete-overlay eth-sentence-overlay)))

(defun eth-highlight-segment (beg end)
  "Highlight the region between BEG and END."
  (interactive)
  (if (overlayp eth-sentence-overlay)
      ;; there is already an overlay: just move it
      (move-overlay eth-sentence-overlay beg end)
    ;; there is no overlay: put a new one
    (let ((overlay (make-overlay beg end)))
      (overlay-put overlay 'face 'eth-highlight-face)
      (setq eth-sentence-overlay overlay))))

;;;;;;;;;;;;;;;;;;;;;
;; VARIOUS HELPERS ;;
;;;;;;;;;;;;;;;;;;;;;
(defun eth-edit-in-buffer (candidate &optional bufname)
  "Display initial translation CANDIDATE in buffer BUFNAME.
Allows then for editing this candidate, and returns the edited translation."
  (let ((main-buffer (buffer-name))
        (edited-string candidate)
        (eth-buffer (if bufname bufname"*[eth]*")))
    (save-excursion
      (switch-to-buffer-other-window eth-buffer)
      (set-buffer eth-buffer)
      (text-mode)
      (visual-line-mode 1)
      (local-set-key (kbd "C-c C-c") 'exit-recursive-edit)
      (when (stringp candidate)
        (insert candidate))
      (message "When you're done editing, press C-c C-c to accept this translation.")
      (unwind-protect
          (recursive-edit)
        (when (get-buffer-window eth-buffer)
          (progn
            (setq edited-string (buffer-string))
            (kill-buffer eth-buffer))))
      (switch-to-buffer main-buffer)
      edited-string)))

(defun eth--edit-candidate (candidate)
  "Display and propose editing of CANDIDATE string.
Return the edited string."
  (if eth-edit-in-minibuffer-p
      (read-from-minibuffer "Edit translation: " candidate)
    (display-buffer-in-side-window
     (generate-new-buffer "*[eth]*")
     '((side . right) (slot . 1)))
    (eth-edit-in-buffer candidate "*[eth]*")))

;;;;;;;;;;;;;;;;;;;;
;; AUTO-TRANSLATE ;;
;;;;;;;;;;;;;;;;;;;;
(defcustom eth-source-language "en"
  "Source language.
See also google-translate-default-source-language")

(defcustom eth-target-language "fr"
  "Target language.
See also google-translate-default-target-language")

(defcustom eth-edit-in-minibuffer-p nil
  "Specify whether the candidate should be displayed and edited in minibuffer.
If t, edit candidate in minibuffer.
If nil, edit candidate in dedicated side-buffer.")

(defun eth-translate-next-sentence ()
  "Translate current or next sentence.
Translate current sentence if point is at the beginning or middle of a sentence.
Translate next sentence if point is at the end of a sentence."
  (interactive)
  (let* ((beg (progn (forward-sentence) (point)))
         (end (progn (backward-sentence) (point)))
         (text-to-translate
          (buffer-substring-no-properties beg end))
         (json (google-translate-request
                eth-source-language
                eth-target-language
                text-to-translate))
         (trans-text (google-translate-json-translation json))
         (edited-text nil))
    (eth-highlight-segment beg end)
    (setq edited-text (eth--edit-candidate trans-text))
    (goto-char beg)
    (save-excursion
      (delete-region beg end)
      (insert edited-text))
    (forward-sentence)))

(provide 'eth)
;;; eth.el ends here
