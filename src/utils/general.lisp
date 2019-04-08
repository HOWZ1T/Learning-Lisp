(defpackage utils
  (:use :cl)
  (:export :list-glob))
(in-package :utils)

(defvar funcs ())
  (defvar vars ())
  (do-external-symbols (s "SB-EXT")
    (when (fboundp s)
      (append (format t "~S | ~%" s) funcs))
    (when (boundp s)
      (append (format t "~S | ~%" s) vars)))
  ("functions: \n")
  (loop for func in funcs do (format t "~%" func))
  ("variables: \n")
  (loop for var in vars do (format t "~%" var)))

(defun list-glob ())
