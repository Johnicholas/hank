;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Various button widgets, designed in Photoshop and imported for
;;; use within our system. These button widgets can be used for
;;; control panels within Froglet or a similar system.

(in-package "HANK")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-image-pathname (name)
    (merge-pathnames name (merge-pathnames (make-pathname 
                                             :directory '(:relative "hank" "images"))
                            *system-root*))))

(defparameter *button-texture-info* 
  '#.(cg:load-texture-info (get-image-pathname "buttonrightup.bmp")))

(defun unpack-array (array)
  (let ((y (array-dimension array 0))
        (x (array-dimension array 1)))
    (loop for i from 0 below y
          collect (loop for j from 0 below x
                        collect (aref array i j)))))

(defmacro define-button (name file)
  (multiple-value-bind (array texture-info) (cg:load-pixmap file)
    (let ((round (floor 32 (cg:texture-info-bits-per-pixel texture-info))))
      `(defparameter ,name
         (make-array '(,(cg:texture-info-height texture-info)
                       ,(* round (ceiling (cg:texture-info-width texture-info) round)))
           :element-type '(mod 16)
           :initial-contents ',(unpack-array array))))))

(define-button right.up #.(get-image-pathname "buttonrightup.bmp"))
(define-button right.down #.(get-image-pathname "buttonrightdown.bmp"))
(define-button right.disabled #.(get-image-pathname "buttonrightdisabled.bmp"))

(define-button fastright.up #.(get-image-pathname "buttonfastrightup.bmp"))
(define-button fastright.down #.(get-image-pathname "buttonfastrightdown.bmp"))
(define-button fastright.disabled #.(get-image-pathname "buttonfastrightdisabled.bmp"))

(define-button left.up #.(get-image-pathname "buttonleftup.bmp"))
(define-button left.down #.(get-image-pathname "buttonleftdown.bmp"))
(define-button left.disabled #.(get-image-pathname "buttonleftdisabled.bmp"))

(define-button fastleft.up #.(get-image-pathname "buttonfastleftup.bmp"))
(define-button fastleft.down #.(get-image-pathname "buttonfastleftdown.bmp"))
(define-button fastleft.disabled #.(get-image-pathname "buttonfastleftdisabled.bmp"))

(define-button stop.up #.(get-image-pathname "buttonstopup.bmp"))
(define-button stop.down #.(get-image-pathname "buttonstopdown.bmp"))
(define-button stop.disabled #.(get-image-pathname "buttonstopdisabled.bmp"))

(defun get-button-texture-info ()
  *button-texture-info*)

(defparameter arrow.up
  (make-array '(26 32) :element-type '(mod 16)
    :initial-contents
      '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 14 14 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 15 15 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 15 15 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 2 2 12 12 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 2 2 13 13 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 2 2 14 13 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 3 3 3 3 1 1 1 1 1 1 1 1 1 2 2 8 8 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 1 1 1 1 1 1 1 1 2 2 9 9 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 3 1 1 1 1 1 1 1 2 2 9 9 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 3 3 3 1 1 1 1 1 1 1 1 1 1 2 2 10 10 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 3 1 3 3 1 1 1 1 1 1 1 1 1 1 2 2 11 11 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 3 1 1 1 3 3 1 1 1 1 1 1 1 1 1 2 2 3 6 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 2 2 7 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 2 2 2 1 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 2 2 2 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
        (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 2 2 2) 
        (0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2) 
        (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3))))

(defparameter arrow.down
  (make-array '(26 32) :element-type '(mod 16)
    :initial-contents 
      '((2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 3 4 3 3 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 3 4 4 4 3 3 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4)
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4)
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4)
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
        (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3))))

(defparameter connect.up
  (make-array '(26 32) :element-type '(mod 16)
    :initial-contents 
      '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 1 2 2 0 44 2 4 4 4) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 2 2 0 0 2 4 4 4) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 1 1 1) 
    (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 3 0 2 2 2) 
    (0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2) 
    (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3))))

(defparameter connect.down
  (make-array '(26 32) :element-type '(mod 16)
    :initial-contents 
      '((2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 0 5 2 1 1 1) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 40 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 43 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 0 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 3 3 4 4 4 4 4 4 4 4 0 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 3 3 3 4 4 4 4 4 4 4 3 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 0 44 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 4 4 4 4 4 4 4 3 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 4 4 4 4 4 4 4 4 5 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 4 4 4 4 4 4 4 4 4 0 0 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 2 4 4 4) 
    (2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 0 0 0 0 0 0))))










