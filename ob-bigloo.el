;;; ob-template.el --- org-babel functions for template evaluation

;; Copyright (C) 2024 Joseph Donaldson

;; Author: Joseph Donaldson
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.5

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

;;; Requirements:

;; You will need to have Bigloo installed on the user's PATH.  It is
;; also necessary to have BEE, the Bigloo Emacs Environment,
;; installed. BEE is provided with the Bigloo compiler.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(require 'bee-mode)

(add-to-list 'org-src-lang-modes '("bigloo" . bee))

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("bigloo" . "scm"))


(defcustom org-babel-bigloo-compiler "bigloo"
  "Command used to compile a Bigloo source code file into an executable.
May be either a command in the path, like \"bigloo\"
or an absolute path name, like \"/usr/local/bin/bigloo\".
Parameters may be used like this: \"bigloo -c\""
  :group 'org-babel
  :type 'string)

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:bigloo '((library . :any)
                                               (flags . :any))
  "Default header arguments for bigloo code blocks.")

(defun ob-bigloo-read-as-list ( str )
  "Read from STR and convert to a list, if it is not already"
  (let ((res (org-babel-read
              str
		   nil)))
    (if (stringp res)
        (list res)
      res)))

(defun ob-bigloo-generate-library-clause (libraries)
  "Generate bigloo library clause with the provided LIBRARIES."
  (if (not (null libraries))
      (concat "(library "
              (mapconcat (lambda (l)
                           (format "%s " l))
                         libraries
                         "")
              ")\n")
    ""))

(defun ob-bigloo-add-vars (vars body)
  "Add VARS to a Bigloo let-block wrapping BODY."
  (if (null vars)
      body
    (format "(let (%s)\n%s\n)"
            (mapconcat
             (lambda (var)
               (format "%S" (print `(,(car var) ',(cdr var)))))
             vars
             "\n")
            body)))

(defun ob-bigloo-generate-body (vars result-type body)
  "Generate the Bigloo code given VARS and BODY targeting RESULT-TYPE."
  (let ((body-w/vars  (ob-bigloo-add-vars vars body)))
    (format "(display %s)"
            (if (eq result-type 'output)
                (format "(with-output-to-string (lambda () %s))"
                        body-w/vars)
              body-w/vars))))
  

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:bigloo' function below.
(defun org-babel-expand-body:bigloo (body params &optional processed-params)
  "Expand BODY according to PARAMS and optional PROCESSED-PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
	(prepends (cdr (assq :prologue params)))
        (result-type (cdr (assq :result-type params)))
        (library (ob-bigloo-read-as-list
		   (cdr (assq :library params)))))
    (concat (format "(module %s %s)"
                    (gensym 'org)
                    (ob-bigloo-generate-library-clause library))
            (and prepends (concat prepends "\n"))
	    (ob-bigloo-generate-body vars result-type body))))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
(defun org-babel-execute:bigloo (body params)
  "Execute a block of Bigloo code represented by BODY and PARAMS with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((tmp-src-file (org-babel-temp-file
			"bigloo-src-"
			".scm"))
         (tmp-bin-file (org-babel-temp-file "bigloo-bin-" org-babel-exeext))
         (cmdline (cdr (assq :cmdline params)))
         (flags (cdr (assq :flags params))))
    (with-temp-file tmp-src-file (insert   (org-babel-expand-body:bigloo body params)))
    (org-babel-eval
     (format "%s %s -o %s %s"
	     org-babel-bigloo-compiler
	     (mapconcat #'identity
			(if (listp flags) flags (list flags)) " ")
	     (org-babel-process-file-name tmp-bin-file)
	     (org-babel-process-file-name tmp-src-file)) "")
    (when (file-executable-p tmp-bin-file)
	(let ((results
	       (org-trim
		(org-babel-eval
		 (concat tmp-bin-file (if cmdline (concat " " cmdline) "")) "")))
              (result-params (cdr (assq :result-params params))))
	  (org-babel-reassemble-table
           (org-babel-result-cond result-params
             (org-babel-read results)   
             (read (org-babel-bigloo-vector-to-list results)))
 	   (org-babel-pick-name
	    (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
	   (org-babel-pick-name
	    (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

(defun org-babel-bigloo-vector-to-list (results)
  "Convert vector RESULTS to list."
  (replace-regexp-in-string "#(" "(" results))

(provide 'ob-bigloo)
;;; ob-bigloo.el ends here
