;;;; This package has general utility functions for doing helpful things in lisp
(defpackage utils
  (:use :cl)
  (:export :print-globs))
(in-package :utils)

;;; print-globs will print out everything in SBCL's global scope
(defun print-globs ()
  (defparameter funcs nil)
  (defparameter vars nil)
  (defparameter word_size 0)
  (do-external-symbols (s "SB-EXT")
    (defparameter sym_str (symbol-name s)) ; getting the symbol name as a string
    (defparameter sym_size (length sym_str)) ; getting the length of the symbol's name
    (if (> sym_size word_size) (setf word_size sym_size)) ; getting the longest symbol name
    (when (fboundp s) ; executes when the symbol a function (aka function bound)
      (push sym_str funcs))
    (when (boundp s) ; executes when the symbol is a variable
      (push sym_str vars)))
  ;; sorting lists
  (sort funcs #'string-lessp)
  (sort vars #'string-lessp)
  ;; outputting to stdout
  ;; see: https://codereview.stackexchange.com/questions/48580/creating-a-repetitive-string-in-common-lisp
  ;; for explanation of format string
  (format t "~%~v@{~A~:*~}" word_size "*")
  (format t "~%~v@{~A~:*~}FUNCTIONS~%" (- (/ word_size 2) (/ 9 2)) " ")
  (format t "~v@{~A~:*~}" word_size "*")
  (loop for fn in funcs do
       (print (find-symbol fn)))
  (format t "~%~%~v@{~A~:*~}" word_size "*")
  (format t "~%~v@{~A~:*~}VARIABLES~%" (- (/ word_size 2) (/ 9 2)) " ")
  (format t "~v@{~A~:*~}" word_size "*")
  (loop for vr in vars do
    (print (find-symbol vr)))
  )
