;;; guess-tex-master.el --- Guess LaTeX Master File
;; 
;; Filename: guess-tex-master.el
;; Description: Guess LaTeX Master File
;; Author: Unknown & Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Dec 12 14:12:47 2011 (-0600)
;; Version:  0.1
;; Last-Updated: Mon Dec 12 14:52:03 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 29
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defgroup guess-TeX-master nil
  "Guess TeX Master from either the open files or files in the
current directory.  Then optionally set the master file via a
local variable."
  :group 'AUCTeX)

(defcustom guess-TeX-master-includes '("input"
                                       "include"
                                       "makerubric")
  "List of known LaTex includes"
  :type '(repeat (string :tag "Include Tag"))
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-from-buffers t
  "Guess LaTeX Master from current buffer?"
  :type 'boolean
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-from-files t
  "After failing to guess the TeX master from buffers, guess LaTeX master from current files? (Requires egrep)"
  :type 'boolean
  :group 'guess-TeX-master)

(defun guess-TeX-master-from-files (filename)
  "Guess TeX master from egrep list of files"
  (let ((egrep (executable-find "egrep"))
        candidates)
    (when egrep
      (setq candidates
            (split-string
             (shell-command-to-string
              (concat egrep
                      " -l \"\\\\\\\\("
                      (mapconcat (lambda(x) x)
                                 guess-TeX-master-includes "|")
                      "){" filename "([.]tex)?}\" *.tex")) "\n" t))
      (if (stringp candidates)
          (symbol-value 'candidates)
        (message "Multiple Candidates: %s" candidates)
        (setq candidates nil)))
    (symbol-value 'candidates)))

(defun guess-TeX-master-from-buffer (filename)
  "Guesses TeX master from open .tex buffers"
  (let (candidate)
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (save-excursion
                  (goto-char (point-min))
                  (when (re-search-forward (concat "\\\\"
                                                   (regexp-opt guess-TeX-master-includes t)
                                                   "{" filename "\\([.]tex\\)?}") nil t)
                    (setq candidate file))))))))
    (symbol-value 'candidate)))

(defun e ()
  (interactive)
  (let (guess-TeX-master-from-buffers)
   (guess-TeX-master (buffer-file-name))))

;;;###autoload
(defun guess-TeX-master ()
  "Guess the master file for FILENAME"
  (let ((candidate nil)
        (filename (file-name-sans-extension (file-name-nondirectory filename))))
    
    (when guess-TeX-master-from-buffers
      (setq candidate (guess-TeX-master-from-buffer filename)))
    (setq candidate (guess-TeX-master-from-files filename))
    (when candidate
      (message "TeX master document: %s" (file-name-nondirectory candidate))
      (set (make-local-variable 'TeX-master) candidate))))

;;;###autoload
(add-hook 'LaTeX-mode-hook 'guess-TeX-master)
(add-hook 'TeX-mode-hook 'guess-TeX-master)

(provide 'guess-tex-master)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; guess-tex-master.el ends here
