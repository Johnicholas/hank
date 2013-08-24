;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University
;;;
;;; Stuff which is used when porting between different platforms. This is
;;; not a complete compatibility file, because we can't do everything here. 
;;; But it should be able to take care of a few operating system features.

(in-package "HANK")

(defmacro normalise-line-breaks (string)
  #+(and ACLPC Windows) `(pc::cr-to-crlf ,string)
  #+Procyon-Common-Lisp string)

(defvar *dialog-background-colour* 
  #+HANK-WIN95-LOOK (cg:make-rgb :red 192 :green 192 :blue 192)
  #-HANK-WIN95-LOOK cg:white)

