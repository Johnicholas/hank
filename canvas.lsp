;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; HANK uses a different approach to handling windows. Instead of adding a layout 
;;; class, HANK uses specialised windows. These manage stuff more directly. We do 
;;; have a canvas-pane, which is where most of the stuff lies directly. 

(defclass canvas (cg:non-refreshing-pane)
  ((drag-pending-p
     :initform nil)
   (drawingp
     :initform t)
   (click-position
     :initform (cg:make-position 0 0))
   (auto-scroll-p 
     :initform t
     :reader canvas-auto-scroll-p
     :initarg :auto-scroll-p)
   (modifiedp
     :initform nil
     :reader canvas-modified-p)))

(comtab:defmenu-command :command-window ((window canvas))
  window)

;;; No need for a generic dispatch on this, and we want them to
;;; be as fast as possible, so we make it into an ordinary macro.
;;; We implicitly get setf expansion through this. 

(defmacro canvas-drawing-p (pane)
  `(slot-value ,pane 'drawingp))

;;; When we have a desired stream units per inch, we can do the conversion on
;;; boxes and positions. We do, of course, need to be careful when we are doing
;;; multiple conversions, but in practice, we can stick to mostly doing the
;;; conversion in a few places at the end. 
;;;
;;; Scale should be implemented more efficiently than this. We can't quite achieve
;;; that, necessarily, because the intermediate value is bigger than a fixnum, or
;;; to small. Even so, we should use integer arithmetic here, not real. 

(defmacro scale (number intended actual)
  `(floor (* ,number ,actual) ,intended))

(defmacro intended-units-per-inch (window)
  (declare (ignore window))
  72)

(defmacro get-screen-units-per-inch (window)
  `(cg:stream-units-per-inch ,window))

(defun ncanvas-to-stream-units (canvas box-or-position)
  (let ((units-per-inch (intended-units-per-inch canvas)))
    (if (null units-per-inch)
        box-or-position
        (let ((actual-units-per-inch (get-screen-units-per-inch canvas)))
          (when (pro:i= actual-units-per-inch units-per-inch)
            (return-from ncanvas-to-stream-units box-or-position))
          (etypecase box-or-position
            (cg:position
              (cg:nmake-position box-or-position
                (scale (cg:position-x box-or-position) units-per-inch actual-units-per-inch)
                (scale (cg:position-y box-or-position) units-per-inch actual-units-per-inch)))
            (cg:box
              (cg:nmake-box box-or-position
                (scale (cg:box-left box-or-position) units-per-inch actual-units-per-inch)
                (scale (cg:box-top box-or-position) units-per-inch actual-units-per-inch)
                (scale (cg:box-right box-or-position) units-per-inch actual-units-per-inch)
                (scale (cg:box-bottom box-or-position) units-per-inch actual-units-per-inch)))
            (integer
              (scale box-or-position units-per-inch actual-units-per-inch)))))))

(defun nstream-to-canvas-units (canvas box-or-position)
  (let ((units-per-inch (intended-units-per-inch canvas)))
    (if (null units-per-inch)
        box-or-position
        (let ((actual-units-per-inch (get-screen-units-per-inch canvas)))
          (when (pro:i= actual-units-per-inch units-per-inch)
            (return-from nstream-to-canvas-units box-or-position))
          (etypecase box-or-position
            (cg:position
              (cg:nmake-position box-or-position
                (scale (cg:position-x box-or-position) actual-units-per-inch units-per-inch)
                (scale (cg:position-y box-or-position) actual-units-per-inch units-per-inch)))
            (cg:box
              (cg:nmake-box box-or-position
                (scale (cg:box-left box-or-position) actual-units-per-inch units-per-inch)
                (scale (cg:box-top box-or-position) actual-units-per-inch units-per-inch)
                (scale (cg:box-right box-or-position) actual-units-per-inch units-per-inch)
                (scale (cg:box-bottom box-or-position) actual-units-per-inch units-per-inch)))
            (integer
              (scale box-or-position actual-units-per-inch units-per-inch)))))))

;;; Forward the redisplay function so that it allows the layout
;;; object to be involved in the decision about how to handle the
;;; display.

(defmethod cg:device-open ((window canvas) options)
  (prog1 (call-next-method)
    (cg:set-cursor window cg:arrow-cursor)
    (when *colour-palette*
      (cg:set-palette window *colour-palette*))
    (cg:set-background-color window *white-colour*)))

;;; Trap from the main event handler for layout panes 
;;; into the layout event handler. We use a special variable
;;; *event-hook* which allows us to rebind the event handler
;;; for layouts temporarily. This might seem rather curiously
;;; odd, but if actually quite effective. 
;;;
;;; This doesn't happen except for mouse events. All other
;;; events are passed on so that we use the comtab system
;;; for windows with layouts. 

(defparameter *event-hook* 'canvas-event)

(defconstant drag-slop 2)

;;; Modified so that the :start-drag event uses the click position for
;;; its data. The mouse-moved event which will follow it will contain 
;;; the real position. This means that a drag called on the :start-drag
;;; event gets the right mouse down position to start its drag. 
;;;
;;; Added handler for character events. These always end up calling the
;;; layout dispatcher *and* the next method. The next method *must* be
;;; called to ensure character translation, as otherwise the messages
;;; vanish into the ether. Don't bother with any position stuff, because
;;; the data slot isn't a position. This addition is so the layout event
;;; loop can detect presses of keys such as the shift key during a 
;;; drag operation. Neat it isn't, but it just might work. Key presses
;;; are even rarer than mouse events, so it shouldn't slow down processing
;;; at all, really. 

(defun call-event-hook (window event buttons data time)
  (funcall *event-hook* window event buttons data time))

(defmethod cg:event ((window canvas) event buttons data time)
  (with-slots (click-position drag-pending-p) window
    (declare (special cg::character))
    (when (or (pro:i= event cg:virtual-key-up) 
              (pro:i= event cg:virtual-key-down) 
              (pro:i= event cg::character))
      (call-event-hook window event buttons data time)
      (call-next-method)
      (return-from cg:event))
    (when (member event '#.(list cg:mouse-left-down cg:mouse-middle-down cg:mouse-right-down))
      (cg:ncopy-position click-position data)
      (setf drag-pending-p t))
    (when (member event '#.(list cg:mouse-left-up cg:mouse-middle-up cg:mouse-right-up))
      (setf drag-pending-p nil))
    (when (or (pro:i= event cg:timer-event))
      #+Procyon-Common-Lisp
      (cg:ncursor-position window data))
    (when (and (pro:i= event cg:mouse-moved) drag-pending-p)
      (let ((click-position click-position))
        (when (or (pro:i> (pro:iabs (pro:i- (cg:position-x data) (cg:position-x click-position))) drag-slop)
                  (pro:i> (pro:iabs (pro:i- (cg:position-y data) (cg:position-y click-position))) drag-slop))
          (setf drag-pending-p nil)
          (call-event-hook window :start-drag buttons click-position time))))
    (if (or (pro:i= event cg:null-event) (pro:i= event cg:mouse-moved) (cg:mouse-event-p event))
        (call-event-hook window event buttons data time)
        (call-next-method))))

(defmethod canvas-event ((window canvas) event buttons data time)
  (declare (ignore window event buttons data time)))

;;; When we get a mouse-moved event for a layout, by default we should set
;;; the cursor to the arrow cursor, in case it isn't currently the arrow
;;; cursor because it has just been moved into the window. This fixes the
;;; problem with the cursor sometimes getting left as an inappropriate
;;; figure in the middle of a window because it inherited it from somewhere
;;; else and nothing ever got around to changing it. 

(defmethod canvas-event ((window canvas) (event (eql cg:mouse-moved)) buttons data time)
  (declare (ignore event buttons data time))
  (cg:set-cursor cg:*screen* cg:arrow-cursor))

;;; By default, a double-click in the standard layout handler should invoke
;;; a command which opens the selection, somehow. This will vary from one
;;; context to another. Only do this when we are double-clicking with the
;;; left mouse button. Note that this applies to the layout, because it isn't
;;; clear that a selection in one layout should the same as a selection in
;;; another. 

(defmethod canvas-event ((window canvas) (event (eql cg:mouse-left-double-click)) buttons data time)
  (declare (ignore event buttons data time))
  (:open-selection window))

(comtab:defmenu-command :open-selection (window)
  (declare (ignore window)))

(defun window-to-screen-units (window box-or-position)
  (let ((scroll-position (cg:nscroll-position window '#.(cg:make-position 0 0))))
    (cg:window-to-screen-units window box-or-position)
    (cg:nposition* scroll-position -1 -1)
    (ctypecase box-or-position
      (cg:box (cg:nbox-move box-or-position scroll-position))
      (cg:position (cg:nposition+ box-or-position scroll-position)))
    box-or-position))

(defun screen-to-window-units (window box-or-position)
  (let ((scroll-position (cg:nscroll-position window '#.(cg:make-position 0 0))))
    (cg:screen-to-window-units window box-or-position)
    (ctypecase box-or-position
      (cg:box (cg:nbox-move box-or-position scroll-position))
      (cg:position (cg:nposition+ box-or-position scroll-position)))
    box-or-position))
