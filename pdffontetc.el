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
(require 'pdf-tools) ;; TO-DO evaluate this dependency
                     ;;  - but probably either need
                     ;;  - or need to duplicate code from pdf-tools
(eval-when-compile
  (require 'org))

;;;; Variables & Configurations

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

;;;; Utility Helpers

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

(defun pdffontetc--merge-cons-to-string (lst)
  "Merge a list \='LST\=' into a white-space separated string."
  (if (null lst) "" (mapconcat #'identity lst " ")))

(defun pdffontetc--resolve-pdf-buffer (doc)
  "Helper to safely resolve DOC path or fallback gracefully."
  (or doc 
      (if (or (pdf-tools-pdf-buffer-p) ;; pdf-tools (poppler)
              (eq major-mode #'reader-mode)) ;; emacs-reader (mupdf)
          (buffer-file-name)
        (read-file-name "Choose PDF file:"))))

;;;; Core Render Engine

(defun pdffontetc--render-org-buffer (buffer-name sections &optional combined)
  "Generic engine to display \='SECTIONS\=' in an Org-mode
temp buffer \='buffer-name\='.
Supports structural grouping overrides via nested lists under
:type `grouped-list'.
SECTIONS is a list of plists containing:
  `(:title String :type (list | table | grouped-list | raw)
 :content Data :headers ListOfStrings)'
The optional argument \='COMBINED\=' is
used when combined with `pdffontetc-display-font-information'."348
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (unless combined (erase-buffer))
      
      ;; org-mode initialization 
      (when (fboundp 'org-mode) (org-mode))
      
      (when (and combined (> (buffer-size) 0))
        (goto-char (point-max))
        (insert "\n\n"))
      
      (dolist (section sections)
        (when section
          (let ((title (plist-get section :title))
                (type (plist-get section :type))
                (content (plist-get section :content))
                (headers (plist-get section :headers)))
            
            ;; Render Section Main Header
            (when title (insert "* " title "\n"))
            
            (cond
             ;; EXIFTOOL GROUPS: format as subheadings ("** ...")
             ((eq type 'grouped-list)
              (dolist (group content)
                (let ((group-heading (car group))
                      (items (cdr group)))
                  ;; prints, e.g.,  "** [XMP-pdf]" cleanly
                  (insert (format "** %s\n" group-heading))
                  (dolist (item items)
                    ;; prints, e.g., "- =PDFVersion=: ~1.7~" cleanly
                    (insert (format "- =%s=: ~%s~\n" (car item) (cdr item))))
                  ;; (insert "\n") ;; don't break
                  ))) 

             ;; STANDARD METADATA LISTS
             ((eq type 'list)
              (dolist (item content)
                (let ((key (car item)) (val (cdr item)))
                  (insert (format "- =%s=: " key))
                  (cond
                   ((null val) (insert "\n"))
                   ((and (listp val) (eq key 'keywords))
                    (insert (mapconcat (lambda (k) (format "~%s~" (string-trim k))) val ", ") "\n"))
                   (t (let ((v (if (listp val) (car val) val)))
                        (if (and (stringp v) (not (string-empty-p v)))
                            (insert (format "~%s~\n" (string-trim v)))
                          (insert "\n"))))))))
             
             ;; FONT TABLES
             ((eq type 'table)
              (let ((table-start (point)))
                (when headers
                  (insert "|" (mapconcat #'identity headers "|") "|\n")
                  (insert "|-\n"))
                (dolist (row content)
                  (insert "|" (mapconcat (lambda (x) (format "%s" (or x ""))) row "|") "|\n"))
                ;; Isolated aligner that works inside org-mode without searching outside the table
                (when (fboundp 'org-table-align)
                  (save-excursion
                    (goto-char table-start)
                    (org-table-align) (org-table-align))))) ;; double-tap
             
             ;; process "raw" text
             ((eq type 'raw)
              (insert content)))
            (insert "\n"))))
      
      ;; unfold all org sections from start
      (when (fboundp 'org-fold-show-all) (org-fold-show-all))
      (read-only-mode 1)
      (unless combined
        (switch-to-buffer-other-window buf))
      (goto-char (point-min)))))


;;;; Backend Data Parsers for PDF Font information

(defun pdffontetc--extract-metadata (doc)
  "Extract standard PDF-Tools meta-pairs from \='DOC\='."
  (mapcar (lambda (item) (cons (car item) (cdr item))) 
          (pdf-info-metadata doc)))

(defun pdffontetc--extract-pdffonts-info (doc)
  "Non-interactive function to parse the output of `pdffonts'.
Extracts information from calling `pdffonts' utility on PDF document
\='DOC\='.  Called by `pdffontetc-display-font-information'."
  (unless (executable-find "pdffonts")
    (error "System package `pdffonts` must be installed"))
  (let* ((cmd (format "pdffonts %s" (shell-quote-argument doc)))
         (raw (remove "" (split-string (shell-command-to-string cmd) "\n")))
         ;; skip header line & dashed separator line safely
         (body (if (> (length raw) 2) (cddr raw) nil))
         (results nil))
    (dolist (line body)
      (let ((tokens (split-string line " " t)))
        ;; ensure we have enough columns to represent standard pdffonts lines
        (when (>= (length tokens) 6)
          (let* ((len (length tokens))
                 ;; object-ID components are always the last two columns
                 (gen-id (nth (- len 1) tokens))
                 (num-id (nth (- len 2) tokens))
                 (object-id (concat num-id "." gen-id))
                 ;; read columns backwards from object ID position
                 (uni (nth (- len 3) tokens))
                 (sub (nth (- len 4) tokens))
                 (emb (nth (- len 5) tokens))
                 (encoding (nth (- len 6) tokens))
                 ;; everything before encoding belongs to font-name and font-type
                 (remaining-prefix (butlast tokens 6))
                 ;; the first element is always the main font name identifier
                 (font-name (or (car remaining-prefix) "[No Name]"))
                 ;; anything left between name and encoding is the font type
                 (type-list (cdr remaining-prefix))
                 (type (if type-list (mapconcat #'identity type-list " ") "[No Type]")))
            (push (list font-name type encoding emb sub uni object-id) results)))))
    (nreverse results)))

(defun pdffontetc--extract-exiftool-accessibility (doc)
  "Extract specific accessibility and validation features
 from \='DOC\=' using ExifTool.
Silences underlying environment localization shell errors safely.
Returns an alist grouped by ExifTool family bracket keys:
  ((\"[XMP-dc]\" . ((\"Title\" . \"Syllabus...\") (\"Creator\" . \"No Mann\")))
   (\"[PDF]\" . ((\"PDFVersion\" . \"1.7\"))))"
  (if (not (executable-find "exiftool"))
      '(("Error" . (("Status" . "exiftool is not installed"))))
    (let* ((flags "-G1 -a -s -u") ;; show *all* PDF info
           (cmd (format "exiftool %s %s 2>/dev/null" flags (shell-quote-argument doc)))
           (raw-lines (split-string (shell-command-to-string cmd) "\n" t))
           (groups nil))
      (dolist (line raw-lines)
        ;; regex matches exactly: "[Group]   Key   : Value" regardless of spacing sizes
        (when (string-match "^\\(\\[[^]]+\\]\\)\\s-+\\([^:]+?\\)\\s-*:\\s-\\(.*\\)$" line)
          (let* ((group-name (match-string 1 line))
                 (tag-key (match-string 2 line))
                 (tag-val (match-string 3 line)))
            (unless (string-empty-p tag-val)
              (let* ((existing-group (assoc group-name groups)))
                (if existing-group
                    (setcdr existing-group (append (cdr existing-group) (list (cons tag-key tag-val))))
                  (push (list group-name (cons tag-key tag-val)) groups)))))))
      (nreverse groups))))


;;;; Interactive User Operations

;;;###autoload
(defun pdffontetc-display-metadata-org-style (&optional doc combined)
  "Display PDF metadata in a separate buffer in Org-mode style.
Argument \='DOC\=' defaults to current buffer if it contains a PDF file;
otherwise queries for a PDF file.  The optional argument \='COMBINED\=' is
used when combined with `pdffontetc-display-font-information'."
       (interactive (list (pdffontetc--resolve-pdf-buffer nil) nil))
       (let* ((target-doc (pdffontetc--resolve-pdf-buffer doc))
              (buf-name (if combined "*PDF metadata and font info* from =epdfinfo= server" "*PDF metadata*"))
              (sections
               (list
                (list :title "Extended PDF metadata from =exiftools=~:")
                :type 'list
                :content (pdffontetc--extract-metadata target-doc))
               ;; Explicitly set :type to 'grouped-list
               (list :title "PDF metadata for file \"=%s=\" from exiftools:"
                     :type 'grouped-list
                     :content (pdffontetc--extract-exiftool-accessibility target-doc))))
         (pdffontetc--render-org-buffer buf-name sections combined)))


;;;###autoload
(defun pdffontetc-display-font-information (&optional doc combined prefix-arg)
  "Parse the output of `pdffonts' for PDF file \='DOC\='.
Information is display in an Org-mode table in a temporary buffer.
Includes explanatory information if called with prefix argument.
\(I.e., if command is preceded by `C-u'.\) Optional \='COMBINED\='
argument alters behaviour for use with
`pdffontetc-display-combined-metadata-and-font-info'."
  (interactive (list (pdffontetc--resolve-pdf-buffer nil) nil current-prefix-arg))
  (let* ((target-doc (pdffontetc--resolve-pdf-buffer doc))
         (buf-name (if combined "*PDF metadata and font info*" "*PDF fonts*"))
         (sections
          (list
           (list :title (format "PDF font information for file \"=%s=\":"
                                (file-name-nondirectory target-doc))
                 :type 'table
                 :headers '("=name=" "=type=" "=encoding=" "=emb=" "=sub=" "=uni=" "=object ID=")
                 :content (pdffontetc--extract-pdffonts-info target-doc)))))
    (when prefix-arg
      (setq sections
            (append sections (list
                              (list
                               :type 'raw :content pdffontetc-pdffonts-man-help)))))
    (pdffontetc--render-org-buffer buf-name sections combined)))


;;;###autoload
(defun pdffontetc-display-combined-metadata-and-font-info (&optional doc prefix-arg)
  "Show combined PDF metadata and font information.
Operates on PDF document \='DOC\=', either current buffer, or passed
manually, or user is queried to supply one.  \(Prefixed argument
triggers showing explanatory information for font metadata.\)"
  (interactive (list (pdffontetc--resolve-pdf-buffer nil) current-prefix-arg))
  (let* ((target-doc (pdffontetc--resolve-pdf-buffer doc))
         (buf-name "*PDF metadata and font info*")
         (master-sections
          (list
           ;; 1. Poppler metadata
           (list :title (format "PDF metadata for file \"=%s=\" from =epdfinfo= server:"
                                (file-name-nondirectory target-doc))
                 :type 'list
                 :content (pdffontetc--extract-metadata target-doc))
           
           ;; 2. Exif metadata
           ;; (explicitly set :type to 'grouped-list)
           (list :title "Extended PDF metadata from =exiftools=:"
                 :type 'grouped-list
                 :content (pdffontetc--extract-exiftool-accessibility target-doc))
           
           ;; 3. pdffontinfo 
           (list :title "PDF Font Information:"
                 :type 'table
                 :headers '("=name=" "=type=" "=encoding=" "=emb=" "=sub=" "=uni=" "=object ID=")
                 :content (pdffontetc--extract-pdffonts-info target-doc)))))
    
    ;; add check for prefix for whether to show help block additions onto the pipeline stack
    (when prefix-arg
      (setq master-sections 
            (append master-sections 
                    (list (list :type 'raw :content pdffontetc-pdffonts-man-help)))))
    
    ;; render everything atomically into the exact same target canvas
    (pdffontetc--render-org-buffer buf-name master-sections nil)))



(provide 'pdffontetc)

;;; pdffontetc.el ends here
