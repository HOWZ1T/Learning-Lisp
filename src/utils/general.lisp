;;;; This package has general utility functions for doing helpful things in lisp
(defpackage utils
  (:use :cl)
  (:export :print-globs))
(in-package :utils)


;;; get-globs returns 3 values that being a sorted list of symbols and the largest word size
;;; PARAMETERS:
;;;     pkg - the package you're getting the globals for (e.g "SB-EXT")
;;; RETURN:
;;;     (funcs: funcs vars: vars word-size: word-size)
(defun get-globs (pkg)
  ;; ensure pkg is uppercase if it is a string
  (if (stringp pkg) (setf pkg (string-upcase pkg)))
  (let ((funcs nil)
	(vars nil)
	(word-size 0))
    (do-external-symbols (s (find-package pkg))
      (let* ((sym-str (symbol-name s))
	     (sym-size (length sym-str)))
	(if (> sym-size word-size)
	    (setf word-size sym-size))
	(when (fboundp s)
	  (if s ; if s is not nil
	      (if (not (member sym-str funcs)) ; if s is not a member of the list, thus ensuring no duplicates
		  (push sym-str funcs))))
	(when (boundp s)
	  (if s ; if s is not nil
	      (if (not (member sym-str vars)) ; if s is not a member of the list, thus ensuring no duplicates
		  (push sym-str vars))))))
    (list :funcs (sort funcs #'string-lessp) :vars (sort vars #'string-lessp) :word-size word-size)))


;; print-header will print a nicely formatted section header
;; PARAMETERS:
;;     title - the title of the section header
;;     width - the total width of the section
;; RETURNS:
;;     nil
(defun print-header (title width)
  ;; see: https://codereview.stackexchange.com/questions/48580/creating-a-repetitive-string-in-common-lisp
  ;; for explanation of format string
  (format t "~%~v@{~A~:*~}" width "*")
  (format t "~%~v{~A~:*~}" (- (/ width 2) (/ (length title) 2)) '(" "))
  (princ title)
  (format t "~%~v@{~A~:*~}" width "*"))


;;; print-globs will print out all externel symbols of the specified package
;;; PARAMETERS:
;;;     pkg - the package you're getting the globals for (e.g "SB-EXT")
;;; RETURNS:
;;;     nil
(defun print-globs (pkg)
  ;; ensure pkg is uppercase if it is a string
  (if (stringp pkg) (setf pkg (string-upcase pkg)))
  (let (raw-list
	funcs
	vars
	word-size)
    (setf raw-list (get-globs pkg))
    (setf funcs (getf raw-list :funcs))
    (setf vars (getf raw-list :vars))
    (setf word-size (getf raw-list :word-size))
    
    ;; outputting to stdout
    ;; printing out global functions
    (print-header "FUNCTIONS" word-size)
    (if (> (length funcs) 0)
	(loop for fn in funcs do
	     (format t "~%")
	     (princ fn)) ; princ prints the string in human-readable format without quotes
	(progn
	  (format t "~%")
	  (princ "NO EXTERNAL FUNCTIONS")))

    ;; add newline separator to separator the previous section from the next one
    (format t "~%")
    
    ;; printing out global variables
    (print-header "VARIABLES" word-size)
    (if (> (length vars) 0)
	(loop for vr in vars do
	     (format t "~%")
	     (princ vr)) ; princ prints the string in human-readable format without quotes
	(progn
	  (format t "~%")
	  (princ "NO EXTERNAL VARIABLES")))) nil) 
