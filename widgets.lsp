;;; -*- Mode: Lisp; Package: COMMON-GRAPHICS -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Various button widgets, implemented using Common Graphics and added
;;; to the Common Graphics package. These are intended to be portable.

(in-package "COMMON-GRAPHICS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'pixmap-button "COMMON-GRAPHICS"))

;;; The window is just a subclass of widget window, with a drawing 
;;; function which presents the button with the right shading and
;;; stuff. Note that we do need to take care of the available-p 
;;; flag.
;;;
;;; =======================================================================
;;; P i x m a p   B u t t o n s 
;;;
;;; :title -- list of three values
;;;             1. a texture info
;;;             2. a pixmap for "up" state
;;;             3. a pixmap for "down" state

(defclass pixmap-button (cg:dialog-item)
  ())

(defclass pixmap-button-window (cg:basic-pane)
  ((dialog-item
     :initarg :dialog-item
     :reader pixmap-button-window-dialog-item)
   (saved-value
     :accessor pixmap-button-window-saved-value)))

(defmethod cg:widget-device ((widget pixmap-button) (dialog cg:basic-pane))
  (declare (ignore item dialog))
  'pixmap-button-window)

;;; The tricky stuff is the stuff that's needed when we draw the button 
;;; in the window. The button is represented as a list of two pixmaps,
;;; one for the "up" position, and one for the "down" position. 

(defmethod cg:redisplay-window ((window pixmap-button-window) &optional box)
  (let* ((item (pixmap-button-window-dialog-item window))
         (title (cg:dialog-item-title item))
         (texture-info (first title))
         (width (cg:texture-info-width texture-info))
         (height (cg:texture-info-height texture-info))
         (box (cg:nmake-box '#.(cg:make-box 0 0 0 0) 0 0 width height)))
    (cg:copy-pixels-to-stream window (cond ((not (cg:dialog-item-available-p item))
                                            (fourth title))
                                           ((cg:dialog-item-value item)
                                            (third title))
                                           (t
                                            (second title)))
      texture-info box box #+ACLPC cg:po-replace #+Procyon-Common-Lisp cg:replace)))

(defmethod cg:widget-set-value ((window pixmap-button-window) (item cg:dialog-item) 
                                new-value old-value recursivep)
  (unless recursivep
    (cg:redisplay-window window)))

#+ACLPC
(defmethod cg:enable-events ((window pixmap-button-window) enable-p)
  (win:EnableWindow (pc:window-handle window) enable-p)
  (when (slot-value window 'cg:dialog-item)
    (cg:invalidate-window window))
  t)

;;; When the mouse goes down in the icon-widget-window we should 
;;; capture the mouse. As long as the mouse is within the window
;;; itself we should set the value to t. Otherwise we should set 
;;; it to nil. All this is dependent on the item being available.
;;;
;;; During tracking, we should save the value and use a copy. When
;;; the mouse is released inside the window, we invoke the value
;;; change for real. 

(defmethod cg:event ((window pixmap-button-window) (event (eql cg:mouse-down)) buttons data time)
  (let ((item (pixmap-button-window-dialog-item window)))
    (when (cg:dialog-item-available-p item)
      (cg:capture-mouse window)
      (setf (pixmap-button-window-saved-value window) (cg:dialog-item-value item))
      (setf (slot-value item 'cg::value) (not (cg:dialog-item-value item)))
      (cg:redisplay-window window))))

(defmethod cg:event ((window pixmap-button-window) (event (eql cg:mouse-double-click)) buttons data time)
  (let ((item (pixmap-button-window-dialog-item window)))
    (when (cg:dialog-item-available-p item)
      (cg:capture-mouse window)
      (setf (pixmap-button-window-saved-value window) (cg:dialog-item-value item))
      (setf (slot-value item 'cg::value) (not (cg:dialog-item-value item)))
      (cg:redisplay-window window))))

(defmethod cg:event ((window pixmap-button-window) (event (eql cg:mouse-up)) buttons data time)
  (let ((item (pixmap-button-window-dialog-item window)))
    (when (cg:dialog-item-available-p item)
      (cg:release-mouse window)
      (let ((displayed-value (slot-value item 'cg::value))
            (saved-value (pixmap-button-window-saved-value window)))
        (unless (eql displayed-value saved-value)
          (setf (slot-value item 'cg::value) saved-value)
          (cg:set-dialog-item-value item displayed-value))))))

;;; Now for the complicated one. We track the mouse and if it is inside
;;; the window we ensure that the new value is displayed, otherwise
;;; we should be displaying the saved value. If the value is going to
;;; change, redisplay the window.

(defmethod cg:event ((window pixmap-button-window) (event (eql cg:mouse-moved)) buttons data time)
  (cg:set-cursor cg:*screen* cg:arrow-cursor)
  (unless (cg:button-match buttons cg:left-mouse-button)
    (return-from cg:event))
  (let ((item (pixmap-button-window-dialog-item window)))
    (when (when (cg:dialog-item-available-p item)
      (let* ((box (cg:nvisible-box window '#.(cg:make-box 0 0 0 0)))
             (displayed-value (slot-value item 'cg::value))
             (saved-value (pixmap-button-window-saved-value window))
             (new-displayed-value nil))
        (setf new-displayed-value (if (cg:inside-box-p data box)
                                      (not saved-value)
                                      saved-value))
        (unless (eql new-displayed-value displayed-value)
          (setf (slot-value item 'cg::value) new-displayed-value)
          (cg:redisplay-window window)))))))

;;; We should add something like a pixmap radio button. This will be 
;;; similar to this, but will behave a bit differently when it is 
;;; already selected. 