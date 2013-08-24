;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; Completely revised drag and drop system for the Windows implementation
;;; of the Ousel workbench. The Procyon model doesn't really work, so I am
;;; going to have to create a hybrid with the Apple model which I can use
;;; to support my needs. This should be able to do everything that the 
;;; Procyon model can, but it should avoid some of the duplication implicit
;;; in that model and provide a little more flexibility.
;;;
;;; The bulk of this is achieved by adding a new class of object, a drag,
;;; which replaces the drag context stuff in the earlier model and is
;;; actually composed of a number of other classes. This means that we
;;; can make the model rather more extensible.

(defclass drag ()
  ((current-position 
     :initform (cg:make-position 0 0)
     :reader drag-current-position)
   (origin 
     :initform (cg:make-position 0 0)
     :reader drag-origin)
   (mouse-down-buttons
     :reader drag-mouse-down-buttons)
   (in-source-window-p
     :initform t
     :reader drag-in-source-window-p)
   (source-window
     :reader drag-source-window)
   (current-window
     :reader drag-current-window)
   (operation
     :reader drag-operation)
   (drop-location
     :accessor drag-drop-location)
   (auto-scroll-p
     :initform t
     :initarg :auto-scroll-p
     :accessor drag-auto-scroll-p)
   (old-tracking
     :accessor drag-old-tracking)
   (new-tracking
     :accessor drag-new-tracking)
   (items
     :initform nil
     :initarg :items
     :accessor drag-items)))

(defclass drag-boxes (drag)
  ((boxes
     :initarg :boxes
     :reader drag-boxes-boxes)))

;;; In the drag there can be many items. Each item is represented
;;; by many types. Each type is associated with a keyword. When a 
;;; drag happens the types are available to the drag receiver to
;;; decide whether it can accept the drag. Each type is derived from
;;; a disembodied property list.
;;;
;;; There are two drag handlers. The first is called during the drag and
;;; the second only when the drag has been completed. It is this second
;;; drag handler that will do whatever we should do as a result of the
;;; drag action. 
;;;
;;; The idea of an "object" which pervaded the Procyon model drag and 
;;; drop handler isn't really there in the Apple model. It was always
;;; slightly application specific, anyway. It was replaced by drag
;;; flavors, which allow several different versions of the data to be
;;; kept in one drag. Each version may have a different data property. 
;;; We will use the Apple model. 
;;;
;;; The drag manager sets up a drag event loop which rebinds the event
;;; loop and sets up a context in which the drag event method will be
;;; called. The drag event loop will drive the drag, by default, but of 
;;; course it can be specialised by subclassing a drag.

(defmethod drag-track ((drag drag))
  (catch 'quit-drag
    (multiple-value-bind (end-buttons end-position)
        (catch 'end-drag
          (let ((*event-hook* #'(lambda (window event buttons data time)
                                  (call-drag-event drag window event buttons data time)))
                (source-window (drag-source-window drag)))
            (declare (special *event-hook*))
            (cg:capture-mouse source-window)
            (drag-start drag)
            (drag-start-tracking drag)
            (unwind-protect (loop
                              (cg:process-events))
              (cg:release-mouse source-window)
              (restore-cursor)
              (drag-end-tracking drag))))
      (drag-end drag end-buttons end-position))))

;;; The rebound event handler will call drag-event, which can dispatch on
;;; most of the parameters. Note, though, that the window that we get
;;; the event sent to is not likely to be the window the moue is actually
;;; over. 
;;;
;;; A lot of this complexity is required to decide what changes in 
;;; window happen. We need this to be able to generate the right
;;; entering and leaving window messages. 

(defmethod drag-event ((drag drag) window event buttons data time)
  (declare (ignore drag window event buttons data time)))

;;; When the mouse is a mouse up event we should quit the drag 
;;; loop at that point. 
;;;
;;; During dragging keep a track of the current window and when this
;;; is different from the previous current window generate a pair of
;;; window entering and leaving events. Most of the time, though, 
;;; we are interested in normal movement operations. 
;;;
;;; Stuff associated with automatic scrolling should also happen 
;;; here. We have to be careful because during the automatic
;;; scroll we might end up moving out of the window. If we do we
;;; should still generate all the window enetering and leaving 
;;; messages. 

(defun drag-handle-window-change (drag new-position)
  (multiple-value-bind (event-window event-position) (get-window-and-position new-position)
    (let ((window (drag-current-window drag)))
      (unless (eq window event-window)
        (drag-leave-window drag window)
        (with-slots (in-source-window-p) drag
          (setf in-source-window-p (eq event-window (drag-source-window drag))))
        (drag-enter-window drag event-window)
        (with-slots (current-window) drag
          (setf current-window event-window))))
    (values event-window event-position)))

;;; This is the core of the dragging system. It is here that everything
;;; to do with an event is handled. We really want to manage things so 
;;; that we don't necessarily accept the mouse position we have been 
;;; given by the system. For example, we might want to "step" the drag, or
;;; pin it into a box in a window. This can be managed by the approval system.
;;; Approval needs more work! Note that dragging will usually be possible on
;;; more than one object, and this needs to be taken into account when approving
;;; and handling other stuff during the drag. 

(defmethod call-drag-event ((drag drag) window event buttons data time)
  (declare (special cg::character))
  (let ((new-position '#.(cg:make-position 0 0))
        (old-position (cg:ncopy-position '#.(cg:make-position 0 0) (drag-current-position drag))))
    (cg:ncursor-position cg:*screen* new-position)
    (multiple-value-bind (event-window event-position) (drag-handle-window-change drag new-position)
      (when (drag-scroll-if-required drag event-window event-position)
        (cg:ncursor-position cg:*screen* new-position)
        (multiple-value-setq (event-window event-position) (drag-handle-window-change drag new-position)))
      (when (or (pro:i= event cg:virtual-key-down)
                (pro:i= event cg:virtual-key-up)
                (pro:i= event cg::character)
                (pro:i= event cg:timer-event)
                (pro:i= event cg:mouse-moved)
                (cg:mouse-event-p event))
      
        ;; We should be a bit smarter than this. Instead of simply using positions, use
        ;; an adjusted "approved" box. This will be the best box approved by the new
        ;; position. Only if the box is different from the old box do we redraw anything. 
        ;; Again, this should use the "approved" box. The old box should always be 
        ;; approved. We can set this up when we start the drag. This will mean we're not
        ;; approving all the time. 
       
        (let ((old-tracking (drag-old-tracking drag))
              (new-tracking (drag-new-tracking drag)))
          (drag-get-approved-tracking drag new-tracking new-position)
          (unless (drag-tracking-equal drag old-tracking new-tracking)
            (drag-hide-tracking drag)
            (drag-copy-tracking drag old-tracking new-tracking)
            (drag-show-tracking drag)
            (cg:ncopy-position (drag-current-position drag) new-position)))
        (drag-event drag event-window event buttons event-position data)))))

(defclass drag-window-mixin ()
  ())

(defmethod call-drag-event ((drag drag-window-mixin) window event buttons data time)
  (declare (special cg::character))
  (let ((new-position '#.(cg:make-position 0 0))
        (old-position (cg:ncopy-position '#.(cg:make-position 0 0) (drag-current-position drag))))
    (if (cg:positionp data)
        (progn
          (cg:ncopy-position new-position data)
          (when (drag-scroll-if-required drag window data)
            (cg:ncursor-position window new-position)))
        (cg:ncursor-position window new-position))
    (when (or (pro:i= event cg:virtual-key-down)
              (pro:i= event cg:virtual-key-up)
              (pro:i= event cg::character)
              (pro:i= event cg:timer-event)
              (pro:i= event cg:mouse-moved)
              (cg:mouse-event-p event))
      
      ;; We should be a bit smarter than this. Instead of simply using positions, use
      ;; an adjusted "approved" box. This will be the best box approved by the new
      ;; position. Only if the box is different from the old box do we redraw anything. 
      ;; Again, this should use the "approved" box. The old box should always be 
      ;; approved. We can set this up when we start the drag. This will mean we're not
      ;; approving all the time. 
       
      (let ((old-tracking (drag-old-tracking drag))
            (new-tracking (drag-new-tracking drag)))
        (drag-get-approved-tracking drag new-tracking new-position)
        (unless (drag-tracking-equal drag old-tracking new-tracking)
          (drag-hide-tracking drag)
          (drag-copy-tracking drag old-tracking new-tracking)
          (drag-show-tracking drag)
          (cg:ncopy-position (drag-current-position drag) new-position)))
      (drag-event drag window event buttons new-position data))))

;;; Trackings are important here. Trackings need to be constructed, and generated from
;;; a position. We should also be able to compare trackings, and to copy them relatively
;;; simply. We can also assume that the "old" tracking has been approved. This may, on
;;; occasion, be a false assumption. 

(defmethod drag-make-tracking ((drag drag-boxes))
  (loop for box in (drag-boxes-boxes drag)
        collect (cg:copy-box box)))

(defmethod drag-copy-tracking ((drag drag-boxes) new-tracking old-tracking)
  (loop for new-box in new-tracking
        for old-box in old-tracking
        do (cg:ncopy-box new-box old-box)))

(defmethod drag-tracking-equal ((drag drag-boxes) tracking1 tracking2)
  (loop for box1 in tracking1
        for box2 in tracking2
        if (not (cg:box= box1 box2))
          do (return nil)
        end
        finally do (return t)))

;;; Dragging can be handled a bit more sensibly than this. We want to turn the drag into
;;; something that can be displayed, but that is specific to the drag class. That way, we
;;; can specialise all the approval system easily. 

(defmethod drag-get-approved-tracking ((drag drag-boxes) new-tracking new-position)
  (loop with delta = (cg:nposition- 
                       (cg:ncopy-position '#.(cg:make-position 0 0) new-position)
                       (drag-origin drag))
        for new-box in new-tracking
        for old-box in (drag-boxes-boxes drag)
        do (cg:ncopy-box new-box old-box)
           (cg:nbox-move new-box delta)))


;;;   (let ((old-box (first (drag-boxes-boxes drag))))
;;;     (ndrag-approve-box (first (drag-items drag)) (cg:nmake-box '#.(cg:make-box 0 0 0 0)
;;;                                                    (cg:box-left old-box) (cg:box-top old-box)
;;;                                                    (cg:position-x new-position) (cg:position-y new-position))
;;;       '#.(cg:make-position 0 0))))

(defmethod drag-event ((drag drag) window event buttons position data)
  (declare (ignore event data))
  (drag-in-window drag window buttons position))

;;; As a special case, when we get a mouse up we should quit the drag
;;; by throwing out to the event loop.

(defmethod drag-event ((drag drag) window (event (eql cg:mouse-left-up)) buttons position data)
  (declare (ignore drag window event data))
  (throw 'end-drag (values buttons position)))

(defmethod drag-event ((drag drag) window (event (eql cg:mouse-right-up)) buttons position data)
  (declare (ignore drag window event data))
  (throw 'end-drag (values buttons position)))

;;; Another special case. We allow the drag to be cancelled by throwing 
;;; out to quit-drag with a flag which requests the drag to be cancelled.
;;; This gets passed as a keyword parameter to drag-end. 

(defmethod drag-event ((drag drag) window (event (eql cg:virtual-key-down)) buttons position data)
  (if (pro:i= data machine:vk-escape)
      (throw 'quit-drag t)
      (call-next-method)))

;;; When the drag is done, restore the mouse cursor to the right cursor for
;;; the current position.
;;;
;;; Bastard Bill Gates!!!

(defun restore-cursor ()
  (multiple-value-bind (window position) (get-window-and-position (cg:ncursor-position cg:*screen* '#.(cg:make-position 0 0)))
    (cg:set-cursor cg:*screen* (cg:cursor window))))

;;; Handling for automatic scrolling. This function looks to see if the mouse
;;; is in a band at the edge of the window. If it is, then we should
;;; automatically scroll by the amount of that band. The following function
;;; should take care of all this.

(defconstant scroll-band-size 16)

;;; When we are about to do some drawing we should hide the current
;;; dragging context. Do this using a macro which should be wrapped 
;;; around drawing and scrolling. 

(defmacro with-drag-drawing-enabled ((drag) &body body)
  (let ((dragsym (gensym)))
    `(let ((,dragsym ,drag))
       (drag-hide-tracking ,dragsym)
       (unwind-protect (progn ,@body)
         (drag-show-tracking ,dragsym)))))

;;; Here I hope that scrolling invokes the redisplay immediately. I am 
;;; fairly sure that it does, and we'll soon find out. Only autoscroll
;;; the window that originated the drag. 
;;;
;;; Note that if we autoscroll we should get a new window and position
;;; because the position will have changed. The method 
;;; layout-drag-scroll-if-required will return t if we autoscrolled. 
;;; This is required because if we autoscroll we need to adjust our 
;;; local position which will now be different. 

(defun drag-scroll (drag window position)
  (unless (drag-auto-scroll-p drag)
    (return-from drag-scroll nil))
  (let ((box (cg:nvisible-box window '#.(cg:make-box 0 0 0 0)))
        (dx 0)
        (dy 0))
    (when (cg:inside-box-p position box)
      (when (< (- (cg:position-x position) (cg:box-left box)) scroll-band-size)
        (decf dx scroll-band-size))
      (when (< (- (cg:position-y position) (cg:box-top box)) scroll-band-size)
        (decf dy scroll-band-size))
      (when (< (- (cg:box-right box) (cg:position-x position)) scroll-band-size)
        (incf dx scroll-band-size))
      (when (< (- (cg:box-bottom box) (cg:position-y position)) scroll-band-size)
        (incf dy scroll-band-size))
      (unless (and (zerop dx) (zerop dy))
        (with-drag-drawing-enabled (drag)
          (cg:scroll window (cg:nmake-position '#.(cg:make-position 0 0) dx dy)))
        t))))

;;; For all classes which aren't really windows we can scroll usefully
;;; then we should fail to scroll and return nil. 
;;;
;;; Added a patch so that layouts which aren't scrollable never go
;;; through to the automatic scrolling code. This means that wells, 
;;; palettes, and the like will never get scrolled. 

(defmethod drag-scroll-if-required ((drag drag) (window canvas) position)
  (when (canvas-auto-scroll-p window)
    (drag-scroll drag window position)))

(defmethod drag-scroll-if-required ((drag drag) (window t) position)
  (declare (ignore drag window position))
  nil)

;;; Finally, we start a drag by specifying a class of drag. This means
;;; specifying stuff about where the origin is and the class of dragging
;;; and all that kind of stuff. By setting up the drag we should 
;;; launch ourselves into the dragging event handler.

(defun make-drag (class window buttons position &rest initargs)
  (let ((instance (apply #'make-instance class initargs)))
    (with-slots (origin current-position mouse-down-buttons source-window current-window) instance
      (cg:ncopy-position origin position)
      (cg:ncopy-position current-position position)
      (setf mouse-down-buttons buttons)
      (setf source-window window)
      (setf current-window window)
      instance)))

;;; Unlike some of the other dragging systems, we make more
;;; extensive use of the cursor during the drag. We use this mainly
;;; as a way of getting feedback during the drag about the operation
;;; that will be selected. 
;;;
;;; This will usually be called in a drag-in-window method. By default,
;;; drag-in-window methods only set the operation to be an invalid
;;; operation. This will apply over all the windows which don't fall into
;;; something more specialised.

(defmethod (setf drag-operation) (value (drag drag))
  (with-slots (operation) drag
    (setf operation value)
;    (cg:set-cursor cg:*screen* (get-cursor value))
    ))

;;; Before we actually get into the drag proper, we should twitch the
;;; tracking by adding the difference between the drag start position and
;;; the current mouse position. Then, and only then, should we show the
;;; tracking images. We should so this at the last minute, before we will
;;; get events, or otherwise it might drift. 

(defmethod drag-start (drag)
  (setf (drag-old-tracking drag) (drag-make-tracking drag))
  (setf (drag-new-tracking drag) (drag-make-tracking drag)))

(defun drag-start-tracking (drag)
  (drag-show-tracking drag))

(defmethod drag-end ((drag drag) buttons position)
  (drag-drop-window drag (drag-current-window drag) buttons position))

(defun drag-end-tracking (drag)
  (drag-hide-tracking drag))

;;; Drag-start and drag-end are left mostly empty. They will usually
;;; be overridden by special classes of drag to do initialisations
;;; that might be required along the way. 
;;;
;;; Some further dispatching for the layout interface. When we have a 
;;; layout pane in the tracking system we forward the message to the
;;; layout. When we have a document layout we forward it to the document.
;;; The following methods handle all this for the three messages that
;;; really count during the drag, drag-in-window, drag-enter-window, 
;;; and drag-leave-window. 

(defmethod drag-in-window ((drag drag) window buttons position)
  (declare (ignore window buttons position))
  (setf (drag-operation drag) nil))

(defmethod drag-enter-window ((drag drag) window)
  (declare (ignore drag window)))

(defmethod drag-leave-window ((drag drag) window)
  (declare (ignore drag window)))

;;; At the end, we need to be able to handle the drop. This is rather
;;; like the window handler, and is forwarded the same way. Again, we
;;; provide the buttons and the position. 

(defmethod drag-drop-window ((drag drag) window buttons position)
  (declare (ignore window buttons position)))

;;; A function which returns the delta for a drag. This is used when
;;; checking the difference between the start of the drag and the 
;;; current drag position.

(defun ndrag-delta (drag position)
  (cg:ncopy-position position (drag-current-position drag))
  (cg:nposition- position (drag-origin drag))
  position)

;;; Usually the tracked image will be a box, so we'll define functions which 
;;; allow us to show and hide the box on the screen. We should probably set the 
;;; texture to be something greyed, too. 

(defconstant equal (+ cg:dst cg:ds~t cg:~d~st cg:~d~s~t))

;;; A few drawing functions which will be useful when we get to the
;;; drawing of tracking regions during the context of the drag 
;;; action. 

(defun drag-track-draw-boxes (boxes drawp)
  (declare (ignore drawp))
  (with-saved-paint-operation (cg:*screen*)
    (cg:set-paint-operation cg:*screen* #+Procyon-Common-Lisp cg:invert #+ACLPC cg:po-invert)
    (set-drag-line cg:*screen*)
    (unwind-protect (map nil #'(lambda (box)
                                 (cg:draw-box cg:*screen* box))
                         boxes)
      (unset-drag-line cg:*screen*))))

(defmethod drag-hide-tracking ((drag drag-boxes))
  (drag-track-draw-boxes (drag-old-tracking drag) nil))

(defmethod drag-show-tracking ((drag drag-boxes))
  (drag-track-draw-boxes (drag-old-tracking drag) t))

;;; We also define a mover. This function will be caled with a new and an
;;; old position. We can subtract to the two to get a relative offset
;;; which we can use to move the box.

(defmethod drag-move-tracking ((drag drag-boxes) new-screen-position old-screen-position)
  (let ((delta (cg:ncopy-position '#.(cg:make-position 0 0) new-screen-position)))
    (cg:nposition- delta old-screen-position)
    (map nil #'(lambda (box)
                 (cg:nbox-move box delta))
         (drag-boxes-boxes drag))))

;;; There are two basic classes of drag in the system. The first is
;;; the one which handles movement, copying, and so on. It will use
;;; box dragging. The current operation, if any, will be nil, :move, 
;;; or :copy. 

(defclass drag-line (drag)
  ((line
     :initarg :line
     :reader drag-line-line)))

(defun drag-track-draw-line (line drawp)
  (declare (ignore drawp))
  (with-saved-paint-operation (cg:*screen*)
    (cg:set-paint-operation cg:*screen* equal)
    (cg:set-line-dashing cg:*screen* :dot)
    (unwind-protect (cg:draw-line cg:*screen* (first line) (second line))
      (cg:set-line-dashing cg:*screen* :solid))))

(defun drag-track-move-line (line new-screen-position old-screen-position)
  (let ((delta (cg:ncopy-position '#.(cg:make-position 0 0) new-screen-position)))
    (cg:nposition- delta old-screen-position)
    (cg:nposition+ (second line) delta)))

(defmethod drag-hide-tracking ((drag drag-line))
  (drag-track-draw-line (drag-line-line drag) nil))

(defmethod drag-show-tracking ((drag drag-line))
  (drag-track-draw-line (drag-line-line drag) t))

(defmethod drag-move-tracking ((drag drag-line) new-screen-position old-screen-position)
  (let ((delta (cg:ncopy-position '#.(cg:make-position 0 0) new-screen-position)))
    (cg:nposition- delta old-screen-position)
    (cg:nposition+ (second (drag-line-line drag)) delta)))

;;; We have one other highlighting function. This draws a circle
;;; centred at a position. We use this to show points on the screen
;;; that links attach to, and things like that. It is an extra kind
;;; of feedback.

(defun drag-track-draw-attachment (position drawp)
  (declare (ignore drawp))
  (with-saved-paint-operation (cg:*screen*)
    (cg:set-paint-operation cg:*screen* equal)
    (let ((box #.(cg:make-box 0 0 0 0))
          (x (cg:position-x position))
          (y (cg:position-y position)))
      (cg:nmake-box box (- x 5) (- y 5) (+ x 5) (+ y 5))
      (cg:fill-box cg:*screen* box))))

;;; Because the event is a mouse event, we can assume that the event
;;; data is a position, and so we can use it to get the corresponding
;;; grid cell. 

(defun call-select-event (window event buttons position context)
  (select-event window event position buttons context))

;;; select-loop is a function which loops forever, but first rebinds
;;; the event handler to a function which allows the context to be accessed
;;; and carries out selection handling during the drag. We should only
;;; forward mouse events during the drag, because keyboard events don't
;;; contain a position and might break the system. 

(defun select-loop (window context)
  (catch 'quit-select
    (let ((*event-hook* #'(lambda (window event buttons data time)
                            (when (or (eq event :start-drag) (cg:mouse-event-p event))
                              (call-select-event window event buttons data context)))))
      (declare (special *event-hook*))
      (cg:capture-mouse window)
      (unwind-protect (loop
                        (cg:process-events))
        (cg:release-mouse window)))))

;;; By default, when we are dealing with a mouse-up event, we can throw
;;; to leave the selection. This will undo most of the things that we
;;; want to be undone, including the temporary rebinding of the event
;;; handler. 

(defmethod select-event ((window canvas) (event (eql cg:mouse-left-up)) position modifiers context)
  (declare (ignore window interaction event position modifiers context))
  (throw 'quit-select ()))

(defmethod select-event ((window canvas) (event (eql cg:mouse-right-up)) position modifiers context)
  (declare (ignore window interaction event position modifiers context))
  (throw 'quit-select ()))

;;; Declare all other events to be ignored during grid selection
;;; handling, so that we don't get too many nasty errors 
;;; happening while we are busy handling this. 

(defmethod select-event ((window canvas) event position modifiers context)
  (declare (ignore interaction event position modifiers context)))

