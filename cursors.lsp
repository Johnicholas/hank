;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University
;;;
;;; Specialised cursor handling for the Mac. We've done this in roughly the same way
;;; that we did on the PC; that is, nastily. This is because the code just doesn't work
;;; in CG for cursor manipulation. Yes, I know it's hard, but it still needs to be 
;;; done. This should set up monochrome cursors tidily. 

(in-package "HANK")

(defconstant adjust-x-bitmap
  '(#B0000000000000000 #B0000000110000000 
    #B0000000110000000 #B0000000110000000 
    #B0000000110000000 #B0001000110001000 
    #B0011000110001100 #B0111111111111110 
    #B0011000110001100 #B0001000110001000 
    #B0000000110000000 #B0000000110000000 
    #B0000000110000000 #B0000000110000000 
    #B0000000000000000 #B0000000000000000))

(defconstant grid-bitmap
  '(#B0000000000000000 #B0000011111000000 
    #B0000010001100000 #B0000010001100000 
    #B0000010001100000 #B0111110001111100 
    #B0100001110000110 #B0100001010000110 
    #B0100001110000110 #B0111110001111110 
    #B0011110001111110 #B0000010001100000 
    #B0000010001100000 #B0000011111100000 
    #B0000001111100000 #B0000000000000000))

(defconstant adjust-x-mask
  '(#B0000001111000000 #B0000001111000000 
    #B0000001111000000 #B0000001111000000 
    #B0000001111000000 #B0011101111011100 
    #B0111111111111110 #B1111111111111111 
    #B0111111111111110 #B0011101111011100 
    #B0001101111011000 #B0000001111000000 
    #B0000001111000000 #B0000001111000000 
    #B0000001111000000 #B0000000000000000))

(defconstant grid-mask
  '(#B0000111111000000 #B0000111111100000 
    #B0000111111110000 #B0000111111110000 
    #B1111111111111100 #B1111111111111110 
    #B1111111111111111 #B1111111111111111 
    #B1111111111111111 #B1111111111111111 
    #B1111111111111111 #B0111111111111111 
    #B0000111111110000 #B0000111111110000 
    #B0000011111110000 #B0000001111110000))

(defconstant pencil-bitmap
  '(#B0000111100000000 #B0000100010000000
    #B0001000010000000 #B0001100100000000
    #B0010011100000000 #B0010001000000000
    #B0100001000000000 #B0100010000000000
    #B1000010000000000 #B1000100000000000
    #B1100100000000000 #B1111000000000000
    #B1110000000000000 #B1100000000000000
    #B1000000000000000 #B0000000000000000))

(defconstant pencil-mask
  '(#B0000111100000000 #B0000111110000000
    #B0001111110000000 #B0001111100000000
    #B0011111100000000 #B0011111000000000
    #B0111111000000000 #B0111110000000000
    #B1111110000000000 #B1111100000000000
    #B1111100000000000 #B1111000000000000
    #B1110000000000000 #B1100000000000000
    #B1000000000000000 #B0000000000000000))

;;; We can also use standard Lisp cursors here; they will be taken from Lisp
;;; as a whole. 

(defconstant cursor-table
  '((:adjust-x nil #.adjust-x-bitmap #.adjust-x-mask 8 8)
    (:grid nil #.grid-bitmap #.grid-mask 7 7)
    (:pencil nil #.pencil-bitmap #.pencil-mask 0 14)
    (:resize nil :system cg:sizing-northwest-southeast-cursor)))
     
;;; This stuff needs to work for both Macs and for PCs. It requires a little
;;; care in porting, therefore. We already have a cursor system for PCs, 
;;; running in Ousel. There are still a few differences to take care of, though. 
;;; PC cursors are usually 32 by 32, and the mask is inverted. 

#+Procyon-Common-Lisp
(defun get-cursor (name)
  (let ((record (assoc name cursor-table)))
    (unless record
      (error "Can't get cursor ~A" name))
    (or (second record)
        (let ((cursor (ct:callocate tb:Cursor)))
          (loop for word in (third record) 
                for index from 0
                do (if (> word #x8000) (decf word #x10000))
                   (setf (ct:cref tb:Cursor cursor (tb:data (fixnum index))) word))
          (loop for word in (fourth record) 
                for index from 0
                do (if (> word #x8000) (decf word #x10000))
                   (setf (ct:cref tb:Cursor cursor (tb:mask (fixnum index))) word))
          (setf (ct:cref tb:Cursor cursor (tb:hotSpot tb:h)) (fifth record))
          (setf (ct:cref tb:Cursor cursor (tb:hotSpot tb:v)) (sixth record))
          (setf (second record) cursor)
          cursor))))

;;; Broadly speaking, this code mostly works, although we must remember to use the cursor
;;; sensibly. In fact, we need to dereference the handle before we use it. This is OK, if 
;;; we are careful. 

#+ACLPC
(defun get-cursor (name)
  (let ((record (assoc name cursor-table)))
    (unless record
      (error "Can't get cursor ~A" name))
    (or (second record)
        (when (eq (third record) :system)
          (setf (second record) (symbol-value (fourth record))))
        (let ((image (make-array '(32 32) :element-type 'bit :initial-element 0))
              (mask (make-array '(32 32) :element-type 'bit :initial-element 1))
              (hotspot (cg:make-position (fifth record) (sixth record))))
          (loop for element in (third record)
                for y from 0
                do (loop for x from 0 below 16
                         for bit = (ldb (byte 1 (- 15 x)) element)
                         do (setf (bit image y x) (- 1 bit))))
          (loop for element in (fourth record)
                for y from 0
                do (loop for x from 0 below 16
                         for bit = (ldb (byte 1 (- 15 x)) element)
                         do (when (pro:izerop bit)
                              (setf (bit image y x) (- 1 (bit image y x))))
                            (setf (bit mask y x) (- 1 bit))))
          (let* ((cursor (cg:make-cursor image hotspot mask))
                 (hotspot (cg::cursor-hot-spot cursor))
                 (hotspot-x (cg:position-x hotspot))
                 (hotspot-y (cg:position-y hotspot))
                 (texture (cg::cursor-texture cursor))
                 (mask (cg::cursor-mask cursor)))
            (setf (second record) cursor)
                    (pc::CreateCursor pc::*hinst* hotspot-x hotspot-y
                      (array-dimension texture 0) (array-dimension texture 1)
                      (acl::%get-pointer mask 4 0) (acl::%get-pointer texture 4 0)))))))
   
