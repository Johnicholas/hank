;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University
;;;
;;; Various textures for HANK. These are designed to be fairly portable and
;;; should be reasonably easy to move from one system to another.

(in-package "HANK")

(setf (getf *textures* :question_box)
        (list 10 10 #'(lambda ()
                        (cg:open-texture cg:*screen*
                          (make-array '(32 32) :element-type 'bit
                            :initial-contents '(#*00000111100000000000000000000000 
                                                #*00001100110000000000000000000000 
                                                #*00001000010000000000000000000000 
                                                #*00001000010000000000000000000000 
                                                #*00001100110000000000000000000000
                                                #*00011111100000000000000000000000 
                                                #*00111000000000000000000000000000 
                                                #*01110000000000000000000000000000 
                                                #*11100000000000000000000000000000 
                                                #*01000000000000000000000000000000))
                          (cg:make-texture-info :width 32 :height 10 :bits-per-pixel 1)))))

(defmacro get-texture (keyword)
  `(let ((value (getf *textures* ,keyword)))
     (when (functionp (third value))
       (setf (third value) (funcall (third value))))
     value))

