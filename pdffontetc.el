;;; pdffontetc.el --- Display `pdffont' and other PDF information -*- lexical-binding: t; -*-

;; pdffontetc - emacs pdffonts info metadata

;; Copyright (C) 2025-2026 Benjamin Slade

;; Author: Benjamin Slade <slade@lambda-y.net>
;; Maintainer: Benjamin Slade <slade@lambda-y.net>
;; URL: https://github.com/emacsomancer/pdffontetc
;; Package-Version: 0.15
;; Version: 0.15
;; Package-Requires: ((emacs "24.4") (pdf-tools "1.2.0"))
;; Created: 2025-03-08
;; Keywords: files, multimedia

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; miscellaneous commands for PDF and PDF font metadata; supplement to PDF-Tools

;;; Installation:
;; To install manually, clone the git repo somewhere and put it in your
;; load-path, e.g., add something like this to your init.el:
;; (add-to-list 'load-path
;;             "~/.emacs.d/pdffontetc/")
;;  (require 'pdffontetc)
;;
;; See README.org for other options.


;;; Usage:
;; - Show PDF metadata in an Org-mode temporary buffer via 'pdffontetc-display-metadata-org-style'. [Advice: bind to (kbd "O")]
;; - Show PDF font innformation in a table in an Org-mode temporary buffer via 'pdffontetc-display-font-information'. Using prefix (= 'C-u' before calling command) to display additional explanatory information/key to pdffonts information. [Advice: bind to (kbd"T")]
;; - Show both via 'pdffontetc-display-combined-metadata-and-font-info'. (Prefix for pdfonts information explainer works here too.) [Advice: bind to (kdb "U")]

;;; Advice:
;; Maybe shadow PDF-Tools' 'pdf-misc-minor-mode-map':
;;
;; (defun pdffontetc-extra-keys ()
;;     "Set some additional keybindings in PDF-Tools for pdffontetc info functions."
;;     ;; 'O' for 'Org-style' Info, = pdf metadata in orgish display:
;;     (local-set-key (kbd "O") #'pdffontetc-display-metadata-org-style)
;;     ;; 'T' for 'Typeface', i.e., Font info [since 'F' is already taken]:
;;     (local-set-key (kbd "T") #'pdffontetc-display-font-information)
;;     ;; 'U' for 'Unified' info, i.e., both Metadata and Font info:
;;     (local-set-key (kbd "U") #'pdffontetc-display-combined-metadata-and-font-info))
;;
;; (add-hook 'pdf-view-mode-hook #'pdffontetc-extra-keys)

;;; Code:

;;;; Requires

;; (require 'pdf-view)
;; (require 'pdf-util)
(require 'pdf-tools)
(eval-when-compile
  (require 'org))

;;;; misc helper functions
(defun pdffontetc--merge-cons-to-string (lst)
  "Merge a list `LST' into a white-space separated string."
  ;; start with first item to avoid leading space
  (let ((unified-field-string (car lst))
        (remain (cdr lst)))
    ;; then process remainder, adding a " " between
    (dolist (w remain)
      (setq unified-field-string
            (concat
             unified-field-string
             " "
             w)))
    unified-field-string))

(defun pdffontetc--flatten-tree (tree)
  "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.

\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7).
[Taken from subr.el to avoid requiring Emacs 27.1]"
  (declare (side-effect-free error-free))
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

;;;; pdf metadata function

;;;###autoload
(defun pdffontetc-display-metadata-org-style (doc &optional combined)
  "Display PDF metadata in a separate buffer in Org-mode style.
Argument `DOC' defaults to current buffer if it contains a PDF file;
otherwise queries for a PDF file.  The optional argument `COMBINED' is
used when combined with `pdffontetc-display-font-information'."
  (interactive
   (if (pdf-tools-pdf-buffer-p)
       (list (buffer-file-name))
     (list (read-file-name "Choose PDF file:"))))
  (let ((raw-pim (pdf-info-metadata doc))
        (temp-buff-name (if combined
                            "*PDF metadata and font info*"
                            "*PDF metadata*")))
    (get-buffer-create temp-buff-name)
    (with-current-buffer temp-buff-name
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "* PDF metadata for file \"=%s=\":\n"
                      (file-name-nondirectory doc)))
      (dolist (item raw-pim)
        (let ((fname (car item))
              (fval (cdr item)))
          ;; keyword field needs special handling
          (if (and (listp fval)
                   (> (length fval) 1)
                   (equal fname 'keywords))
              (let ((kword1 (cadr fval))
                    (kwords (cddr fval)))
                (insert (format "- =%s=: " fname))
                (insert (format "~%s~"
                                (string-trim kword1)))
                (dolist (kword kwords)
                  (insert
                   (format ", ~%s~"
                           (string-trim kword))))
                (insert "\n"))
            (insert (format "- =%s=: " fname))
                      
            (if (or (null fval) ;; don't format empty values or strings
                    (and (and (stringp fval) (string-empty-p fval))))
                (insert "\n")
              ;; if not empty, then check
              (if (stringp fval)               ;; make sure the object is a string
                  ;; if so, then format it:
                  (insert (format "~%s~\n"
                                  (string-trim fval)))
                ;; otherwise, if it's a list instead,
                ;; try car'ing through the apparent list,
                ;;  until we've reached a non list item
                (while (listp fval)
                  (setq fval (car fval)))

                ;; TODO: check for edge cases here? (for symbols?)
                (if (not (stringp fval))  ;; if this isn't a string,
                    (insert "\n")        ;; leave the value blank
                  ;; but if it indeed is a non-empty string
                  (if (not (string-empty-p fval))
                      ;; then format it
                      (insert (format "~%s~\n"
                                      (string-trim fval)))
                    ;; otherwise, if it's an empty string at the end of all this
                    (insert "\n")  ;; leave value blank
                    )))))))
    (org-mode)
      (org-fold-show-all)
      (read-only-mode 1)
      (when (null combined)
        (switch-to-buffer-other-window temp-buff-name))
      (goto-char (point-min)))))

;;;; PDF Font information

(defvar pdffontetc-pdffonts-man-help
  "** Key to the above font information:
*** The following information is listed for each font:
  - =name=: the font name, exactly as given in the PDF file (potentially including
    a subset prefix)
  - =type=: the font type -- see below for details
  - =emb=: \"yes\" if the font is embedded in the PDF file
  - =sub=: \"yes\" if the font is a subset
  - =uni=: \"yes\" if there is an explicit ~ToUnicode~ map in the PDF file (the
    absence of a ~ToUnicode~ map doesn't necessarily mean that the text can't be
    converted to Unicode)
  - =object ID=: the font dictionary object ID (number and generation; given here
    in format ~Number.Generation~)

*** PDF files can contain the following types of fonts:
   - ~Type 1~
   - ~Type 1C~ [= Compact Font Format (CFF)]
   - ~Type 3~
   - ~TrueType~
   - ~CID Type 0~ [= 16-bit font with no specified type]
   - ~CID Type 0C~ [= 16-bit PostScript CFF font]
   - ~CID TrueType~ [= 16-bit TrueType font]

[ adapted from ~man pdffonts~ ]"
  "Key to font data information.
Information about the PDF font information displayed by
`pdffontetc-display-font-information'.")

(defun pdffontetc--extract-pdffonts-info (doc)
  "Non-interactive function to parse the output of `pdffonts'.
Extracts information from calling `pdffonts' utility on PDF document
`DOC'.  Called by `pdffontetc-display-font-information'."
  (unless (executable-find "pdffonts")
    (error "System package `pdffonts' must be installed"))
  (let* ((raw
          (remove ""
                 (split-string
                  (shell-command-to-string
                   (concat "pdffonts \"" doc "\""))
                  "\n")))
         (body (cddr raw))
         (pdffont-values nil))
    (dolist (line body)
      (let ((raw-font-info (nreverse
                            (split-string line))))
        (let* ((object-id (concat
                           (nth 1 raw-font-info)
                           "."
                           (nth 0 raw-font-info)))
               (uni (nth 2 raw-font-info))
               (sub (nth 3 raw-font-info))
               (emb (nth 4 raw-font-info))
               (encoding (nth 5 raw-font-info))
               (remainder (cdddr (cdddr raw-font-info)))
               (flipback (nreverse remainder))
               (font-name (car flipback))
               (type
                (pdffontetc--merge-cons-to-string
                 (pdffontetc--flatten-tree
                       (cdr flipback)))))
          (setq pdffont-values
                (cons
                 (list
                  font-name
                  type
                  encoding
                  emb
                  sub
                  uni
                  object-id)
                 pdffont-values)))))
    pdffont-values))

;;;###autoload
(defun pdffontetc-display-font-information (doc &optional combined)
  "Parse the output of `pdffonts' for PDF file `DOC'.
Information is display in an Org-mode table in a temporary buffer.
Includes explanatory information if called with prefix argument.
\(I.e., if command is preceded by `C-u'.\) Optional `COMBINED' argument
alters behaviour for use with
`pdffontetc-display-combined-metadata-and-font-info'."
  (interactive
   (if (pdf-tools-pdf-buffer-p)
       (list (buffer-file-name))
     (list (read-file-name "Choose PDF file:"))))
  (unless (executable-find "pdffonts")
    (error "System package `pdffonts' must be installed"))
  (let ((pdffont-values (pdffontetc--extract-pdffonts-info doc))
        (temp-buff-name (if combined
                            "*PDF metadata and font info*"
                          "*PDF fonts*")))
    (when (null combined)
      (get-buffer-create temp-buff-name))
    (with-current-buffer temp-buff-name
      (read-only-mode -1)
      (if combined
          (progn
            (goto-char (point-max))
            (insert "\n"))
        (erase-buffer))
      (insert (format "* PDF font information for file \"=%s=\":\n"
                      (file-name-nondirectory doc)))
      (insert "|-\n")
      (let ((header '("=name=" "=type=" "=encoding=" "=emb=" "=sub=" "=uni=" "=object ID=")))
          (dolist (field header)
            (insert " | ")
            (insert field)))
      (insert "|\n")
      (insert "|--")
      (insert "\n")
      (goto-char (point-max))
      (dolist (item pdffont-values)
        (let ((ffield t))
          (dolist (field item)
            (insert " | ")
            (if ffield
                (progn
                  (insert (format "~%s~" field))
                  (setq ffield nil))
              (insert (format "%s" field)))))
        (insert "\n"))
      (insert "|-")
      (org-mode)
      (org-fold-show-all)
      (org-table-align)
      (org-table-align) ; needs to be twice to get formatting right
      (goto-char (point-max))
      (when current-prefix-arg
        (insert "\n")
        (insert pdffontetc-pdffonts-man-help))
      (read-only-mode 1)
      (switch-to-buffer-other-window temp-buff-name)
      ;; visual-fill-column-mode will be too narrow
      ;; disable if on:
      (when (and (boundp 'visual-fill-column-mode)
             visual-fill-column-mode)
        (visual-fill-column-mode -1))
      (goto-char (point-min)))))

;;;; Combined PDF metadata and font information display

;;;###autoload
(defun pdffontetc-display-combined-metadata-and-font-info (doc)
  "Show combined PDF metadata and font information.
Operates on PDF document `DOC', either current buffer, or passed
manually, or user is queried to supply one.  \(Prefixed argument
triggers showing explanatory information for font metadata.\)"
  (interactive
   (if (pdf-tools-pdf-buffer-p)
       (list (buffer-file-name))
     (list (read-file-name "Choose PDF file:"))))
  (pdffontetc-display-metadata-org-style doc t)
  (pdffontetc-display-font-information doc t))

(provide 'pdffontetc)

;;; pdffontetc.el ends here
