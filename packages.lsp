;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University
;;;
;;; Packages for HANK. This is loaded when we're still in the COMMON-LISP-USER
;;; package, and it should be portable. It should set up all the packages we 
;;; need, so that they can be used from now on. We use our old trick of the
;;; public package, so that we can define reading and writing and still build
;;; a sensible run time system. 

(in-package "COMMON-LISP-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter common-lisp-user::public-symbol-names
    '("LOAD-SYSTEM" "LOAD-MODULE"))
   
  (defparameter user::public-lisp-symbol-names 
    (remove-if-not #'(lambda (symbol)
                       (multiple-value-bind (symbol found) (find-symbol symbol "COMMON-LISP")
                         found))
                   common-lisp-user::public-symbol-names))
  (defparameter user::public-local-symbol-names 
    (remove-if #'(lambda (symbol)
                   (multiple-value-bind (symbol found) (find-symbol symbol "COMMON-LISP")
                     found))
               common-lisp-user::public-symbol-names))

;;; PUBLIC is the package that will be visible in a completed agent system.
;;; All the symbols that should be visible in files, or in text, should be
;;; declared in this package, by one means or another. 

  (unless (find-package "PUBLIC")
    (make-package "PUBLIC" :use '()))

  (import (loop for symbol in common-lisp-user::public-lisp-symbol-names
                collect (intern symbol "COMMON-LISP"))
    "PUBLIC")
  
  (export (loop for symbol in common-lisp-user::public-symbol-names
                collect (intern symbol "PUBLIC"))
    "PUBLIC")
   
  (unless (find-package "HANK")
    (make-package "HANK" :use '("COMMON-LISP" "PUBLIC"))))

;;; This can only be evaluated when the PUBLIC package has been
;;; properly created. 

#+ACLPC
(defpackage "PC"
  (:nicknames "MACHINE"))

