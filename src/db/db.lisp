(defpackage db
  (:use :cl :utils)
  (:export :make-cd
	   :add-record
	   :pprint-db
	   :add-cds
	   :save-db
	   :load-db))
(in-package :db)


;;; NON-EXPORTED PACKAGE GLOBAL VARIABLES BELOW
;;; database for storing cds
(defvar *db* nil)


;;; PRIVATE FUNCTIONS BELOW


;;; prompt-for-cd guides the user through creating a cd
;;; RETURNS
;;;     list - in the form (:title :artist :rating :ripped)
(defun prompt-for-cd ()
  (make-cd
   (utils:input "Title")
   (utils:input "Artist")
   (or (parse-integer (utils:input "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))


;;; PUBLIC FUNCTIONS BELOW


;;; make-cd returns a list representing data about the cd
;;; PARAMETERS:
;;;     title - the title of the cd
;;;     artist - the artist of the cd
;;;     rating - the rating of the cd
;;;     ripped - whether the cd is ripped or not
;;; RETURNS:
;;;     list - in the form (:title :artist :rating :ripped)
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))


;;; add-record pushes the specified cd into the database
;;; PARAMETERS:
;;;     cd - a list in the form (:title :artist :rating :ripped) representing the cd
;;; RETURNS:
;;;     nil
(defun add-record (cd)
  (push cd *db*))


;;; add-cds guides the user through entering multiple cds into the database
;;; RETURNS:
;;;     nil
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))


;;; pprint-db pretty prints the database to stdout
;;; RETURNS:
;;;     nil
(defun pprint-db ()
  ;; format iterates over *db* and then over each cd in *db* printing each part of cd 
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))


;;; saves the database to the specified filename
;;; PARAMETERS:
;;;     filename - the path to the output file
;;; RETURNS:
;;;     nil
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))) nil)


;;; loads the database from the specified filename
;;; PARAMETERS:
;;;     filename - the path to the input file
;;; RETURNS:
;;;    nil
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))) nil)
