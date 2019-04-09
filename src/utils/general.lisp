;;;; This package has general utility functions for doing helpful things in lisp
(defpackage utils
  (:use :cl)
  (:export :print-globs))
(in-package :utils)

;;; get-globs returns 3 values that being a sorted list of symbols and the largest word size
;;; return form -> (funcs: funcs vars: vars word-size: word-size)
(defun get-globs ()
  (let ((funcs nil) (vars nil) (word-size 0))
    (do-external-symbols (s "SB-EXT")
      (let (sym-str sym-size)
	(setf sym-str (symbol-name s))
	(setf sym-size (length sym-str))
	(if (> sym-size word-size) (setf word-size sym-size))
	(when (fboundp s)
	  (push sym-str funcs)
	  )
	(when (boundp s)
	  (push sym-str vars)
	  )
	)
      )
    (list :funcs (sort funcs #'string-lessp) :vars (sort vars #'string-lessp) :word-size word-size))
  )

;;; print-globs will print out everything in SBCL's global scope
;;; returns -> nil
(defun print-globs ()
  (let (raw-list funcs vars word-size)
    (setf raw-list (get-globs))
    (setf funcs (getf raw-list :funcs))
    (setf vars (getf raw-list :vars))
    (setf word-size (getf raw-list :word-size))
    ;; outputting to stdout
    ;; see: https://codereview.stackexchange.com/questions/48580/creating-a-repetitive-string-in-common-lisp
    ;; for explanation of format string
    ;; printing out global functions
    (format t "~%~v@{~A~:*~}" word-size "*")
    (format t "~%~v@{~A~:*~}FUNCTIONS~%" (- (/ word-size 2) (/ 9 2)) " ")
    (format t "~v@{~A~:*~}" word-size "*")
    (loop for fn in funcs do
	 (print (find-symbol fn)))

    ;; printing out global variables
    (format t "~%~%~v@{~A~:*~}" word-size "*")
    (format t "~%~v@{~A~:*~}VARIABLES~%" (- (/ word-size 2) (/ 9 2)) " ")
    (format t "~v@{~A~:*~}" word-size "*")
    (loop for vr in vars do
	 (print (find-symbol vr)))
    )
  )
