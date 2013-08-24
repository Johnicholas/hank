;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

;;; Predefinition of a few things that are referred to elsewhere in the
;;; system. This shuts the compiler up in a few places, and things like 
;;; that. 

(defgeneric (setf :modified-p) (value window))
(defgeneric (setf graph-element-selected-p) (value element &optional recursivep))
(defgeneric (setf graph-element-area) (value graph-element &optional recursivep))


