;;; guess-tex-master.el --- Guess LaTeX Master File -*- lexical-binding: t; -*-
;; 
;; Filename: guess-tex-master.el
;; Description: Guess LaTeX Master File
;; Author: Unknown & Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Dec 12 14:12:47 2011 (-0600)
;; Version:  0.4
;; Last-Updated: Mon Dec 12 15:31:35 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 56
;; URL: https://github.com/mlf176f2/guess-tex-master.el
;; Keywords: AucTeX TeX-master
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
;; 12-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Mon Dec 12 15:17:17 2011 (-0600) #55 (Matthew L. Fidler)
;;    Bugfix
;; 12-Dec-2011    Matthew L. Fidler
;;    Last-Updated: Mon Dec 12 14:55:15 2011 (-0600) #31 (Matthew L. Fidler)
;;    Initial release
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
  "Guess TeX Master from either the open buffers or local files.
Then optionally set the master file via a local variable."
  :group 'AUCTeX)

(defcustom guess-TeX-master-includes '("import"
                                       "input"
                                       "include"
                                       "subimport"
                                       "makerubric")
  "List of known LaTex includes.
Supports both commands that directly input the file such as input or commands
which take the path as a second argument such as import."
  :type '(repeat (string :tag "Include Tag"))
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-from-buffers t
  "Guess LaTeX Master from currently open buffers."
  :type 'boolean
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-from-files t
  "Guess LaTeX Master from local files.
Only applies if the guess-TeX-master-from-buffers fails.  Requires
grep."
  :type 'boolean
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-from-files-up 0
  "How many directory levels above current to start searching through files.
The find executable is used to find .tex files, this variable controls how
many ../ are inserted in the find path.  The guess-TeX-master-from-files-depth
should probably be increased as well when changing this value."
  :type 'natnum
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-from-files-depth 1
  "Maximum depth when searching through files.
This controls the -maxdepth option of the find call.  Note that changing the
guess-TeX-master-from-files-up variable influences the find path;
guess-TeX-master-from-files-depth should probably be increased as well so the
start folder is still among the searched objects."
  :type 'natnum
  :group 'guess-TeX-master)

(defcustom guess-TeX-master-default t
  "Default guess if all else fails.
Only applies if the guess-TeX-master-from-buffers and
guess-TeX-master-from-files both fail.  Same choices as TeX-master variable."
  :type '(choice (const :tag "Query" nil)
                 (const :tag "This file" t)
                 (const :tag "Shared" shared)
                 (const :tag "Dwim" dwim)
                 (string :format "%v"))
  :group 'guess-TeX-master)

(defvar TeX-master)

(defun guess-TeX-master-from-files (filename)
  "Guess TeX master for FILENAME from local files using find and grep.
Will execute find in guess-TeX-master-from-files-up directories above FILENAME
with a -maxdepth of guess-TeX-master-from-files-depth.  Greps all .tex files
that were found for includes that match FILENAME and returns the first candidate
that matches."
  (let ((candidate)
        (files-list)
        (files-path))
    (when (and (executable-find "find") (executable-find "grep"))
      (setq files-path
            (make-string
             (+ 1 (* 3 guess-TeX-master-from-files-up)) ?.))
      (dotimes (i guess-TeX-master-from-files-up)
        (store-substring files-path (+ 1 (* i 3)) "/"))
      (setq files-list
            (split-string
             (shell-command-to-string
              (concat "find "
                      files-path
                      " -maxdepth "
                      (number-to-string guess-TeX-master-from-files-depth)
                      " -type f -name \"*.tex\""))
             "\n" t))
      (when files-list
        (dolist (file files-list)
          (unless candidate
            (let (includes-list)
              (setq includes-list
                    (split-string
                     (shell-command-to-string
                      (concat "grep -Eo \"[\\]("
                              (mapconcat (lambda(x) x)
                                         guess-TeX-master-includes "|")
                              "){[^}]*(}{)?"
                              (file-name-sans-extension (file-name-nondirectory filename))
                              "([.]tex)?\\\"?}\" \""
                              file
                              "\""))
                     "\n" t))
              (when includes-list
                (dolist (include includes-list)
                  (unless candidate
                    (string-match (concat "\\\\"
                                          (regexp-opt guess-TeX-master-includes t)
                                          "{\\([^}]*\\)\\(}{\\)?"
                                          (file-name-sans-extension (file-name-nondirectory filename))
                                          "\\([.]tex\\)?\\\"?}")
                                  include)
                    (when (string= filename
                                   (file-truename (string-replace "\"" ""
                                                                  (concat (file-name-directory filename)
                                                                          (file-name-directory file)
                                                                          (match-string 2 include)
                                                                          (file-name-nondirectory filename)))))
                      (setq candidate file))))))))))
    candidate))

(defun guess-TeX-master-from-buffers (filename)
  "Guess TeX master for FILENAME from open .tex buffers."
  (let (candidate)
    (save-excursion
      (dolist (buffer (buffer-list))
        (unless candidate
          (with-current-buffer buffer
            (let ((file buffer-file-name))
              (when (and file (string-match "\\.tex$" file))
                (save-excursion
                  (goto-char (point-min))
                  (while (and (not candidate)
                              (re-search-forward (concat "\\\\"
                                                         (regexp-opt guess-TeX-master-includes t)
                                                         "{\\([^}]*\\)\\(}{\\)?"
                                                         (file-name-sans-extension (file-name-nondirectory filename))
                                                         "\\([.]tex\\)?\\\"?}") nil t))
                    (when (string= filename
                                   (file-truename (string-replace "\"" ""
                                                                  (concat (file-name-directory file)
                                                                          (match-string 2)
                                                                          (file-name-nondirectory filename)))))
                      (setq candidate file))))))))))
    candidate))

;;;###autoload
(defun guess-TeX-master ()
  "Guess the master file for current buffer provided TeX-master is non-nil.
Will check buffers, then files, then the TeX-master variable.  Sets a local
variable TeX-master according to the guess."
  (unless TeX-master
    (let ((candidate nil)
          (filename (buffer-file-name)))
      (when guess-TeX-master-from-buffers
        (setq candidate (guess-TeX-master-from-buffers filename)))
      (unless candidate
        (when guess-TeX-master-from-files
          (setq candidate (guess-TeX-master-from-files filename))))
      (unless candidate
        (setq candidate guess-TeX-master-default))
      (when (stringp candidate)
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
      (set (make-local-variable 'TeX-master) candidate))))

;;;###autoload
(add-hook 'LaTeX-mode-hook 'guess-TeX-master)
;;;###autoload
(add-hook 'TeX-mode-hook 'guess-TeX-master)

(provide 'guess-tex-master)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; guess-tex-master.el ends here
