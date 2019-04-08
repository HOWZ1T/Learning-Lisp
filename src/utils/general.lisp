(defpackage utils
  (:use :cl)
  (:export :print-globs))
(in-package :utils)

(defun print-globs ()
  (defvar funcs nil)
  (defvar vars nil)
  (do-external-symbols (s "SB-EXT")
    (when (fboundp s)
      (push s funcs))
    (when (boundp s)
      (push s vars)))
  (format t "FUNCTIONS:~%")
  (format t "----------------------------------")
  (loop for fn in funcs do
       (print fn))
  (format t "~%~%VARIABLES:~%")
  (format t "----------------------------------")
  (loop for vr in vars do
       (print vr)))
