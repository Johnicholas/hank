;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; Tracer stuff. This is used by the event handling code, but has been moved here
;;; because there was too much of it to fit in that file. That is no real problem,
;;; as the interface between the two is, in fact, pretty small and clear.
;;;
;;; We really need some mapping between the state and the tracer. This is needed so we
;;; can get back to change elements later on. How we manage this is really up to us. 

(defun trace-element (window element)
  (rest (assoc element (cg:get-stream-prop window 'element-table))))

(defun (setf trace-element) (value window element)
  (let* ((table (cg:get-stream-prop window 'element-table))
         (match (assoc element table)))
    (if match
        (setf (rest match) value)
        (cg:set-stream-prop window 'element-table (acons element value table)))
    value))

;;; There should be a few preferences that affect the tracer, if we are to do things
;;; properly. These preferences can be set from within Hank, but we may also define
;;; them through dialogs. Here they are represented as variables, and can be set 
;;; accordingly. Somewhere, then, we'll need to map into these preferences. Somehow. 

(defparameter *tracer-question-seek-mode* :value)
(defparameter *tracer-ask-seek-mode* :value)

(defparameter *tracer-remove-completed-instruction-cards-p* nil)

;;; Horizontal folding is now likely to become compulsory.  Basically, we need to
;;; do this to make the interpreter behave properly.  If you go back to a previous
;;; step, bindings which were added in later steps are automatically faded out,
;;; although values of old ones are retained.  This is fairly easy to implement
;;; because all we really need to record are the new bindings added at each step.  
;;; The values for these bindings can then be dropped when we pop steps off the
;;; stack.  

(defmacro trace-element-height ()
  `(pro:i+ *table-border-size* (pro:i* 4 *grid-row-height*)))

(defun divide-columns (width n)
  (loop for i from n downto 1
        for below = (pro:i/ width i)
        if (pro:i= i 1)
          collect width
        else
          collect below
        end
        do (pro:idecf width below)))

;;; trace-ensure-visible does some kind of autoscrolling, but not particularly well.
;;; This needs a bit more work before it is as neat as it would be. For now, though,
;;; this will do. 

(defun trace-ensure-visible (window box)
  (let ((visible-box (cg:nvisible-box window '#.(cg:make-box 0 0 0 0))))
    (unless (cg:inside-box-p
              (cg:nmake-position '#.(cg:make-position 0 0) (cg:box-left box) (cg:box-top box))
              visible-box)
      (let ((position '#.(cg:make-position 0 0)))
        (if (pro:i> (cg:box-top box) (cg:box-top visible-box))
            (setf (cg:position-y position) (pro:imax 0 (pro:i- (cg:box-top box) 5)))
            (setf (cg:position-y position) (pro:imax 0 (pro:i- (cg:box-bottom box) (cg:box-height visible-box) 
                                                        -5))))
        (if (pro:i> (cg:box-left box) (cg:box-left visible-box))
            (setf (cg:position-x position) (pro:imax 0 (pro:i- (cg:box-left box) 5)))
            (setf (cg:position-x position) 
                    (pro:imax 0 (pro:i- (cg:box-right box) (cg:box-width visible-box) -5))))
        (when (pro:i< (cg:position-x position) 150)
          (setf (cg:position-x position) 0))
        (cg:scroll-to window position)))))

(defmacro trace-element-y-distance ()
  `(pro:i+ *trace-element-row-space* (trace-element-height)))

(defun set-workspace-element-area (element x y)
  (setf (slot-value element 'area) (cg:make-box 0 0 0 0))
  (setf (getf (graph-element-properties element) 'trace-position) (list x y))
  (let* ((element-area (calculate-table-element-area element))
         (left (if (pro:izerop x)
                   *trace-element-inset*
                   (pro:i+ *trace-element-inset* 30 *trace-element-header*
                           (pro:i* (pro:i1- x) (pro:i+ *trace-element-width* *trace-element-status-width*)))))
         (right (pro:i+ left (if (pro:izerop x) *trace-element-header* *trace-element-width*)))
         (top (+ #.(pro:i/ *trace-element-row-space* 2) (pro:i* (trace-element-y-distance) y)))
         (bottom (pro:i+ top (cg:box-height element-area))))
    (setf (graph-element-area element t)
            (cg:make-box left top right bottom))))

(defun install-element (element graph-window)
  (setf (slot-value element 'window) graph-window)
  (setf (slot-value element 'parent) graph-window)
  (push element (graph-elements graph-window)))

(defun tracer-get-status-box (element)
  (let* ((element-area (graph-element-area element))
         (top (cg:box-top element-area))
         (height (pro:i- (cg:box-bottom element-area) top))
         (offset (pro:i/ (pro:i- height *grid-row-height*) 2)))
    (cg:make-box (cg:box-right element-area) 
                 (pro:i+ (cg:box-top element-area) offset)
                 (pro:i+ (cg:box-right element-area) *trace-element-status-width*)
                 (pro:i+ (cg:box-top element-area) offset *grid-row-height*))))

(defun tracer-make-seek (ask question mode)
  (ecase mode
    ((:wildcard) (question-seek question))
    ((:binding)
     (let ((original-seek (ask-original-seek ask))
           (seek (question-seek question)))
       (if original-seek
           (loop for element in seek
                 for original-element in original-seek
                 if (pro:whole-string-equal element original-element)
                   collect element
                 else
                   collect (concatenate 'string element " = " original-element))
           seek)))
    ((:value) (loop with environment = (ask-environment ask)
                    for element in (question-seek question)
                    collect (instantiate-element environment element)))))
  
(defun trace-event-add-ask (trace)
  (let* ((state (trace-state trace))
         (new-ask (first (state-asks state)))
         (new-ask-number (ask-number new-ask))
         (new-ask-question (ask-question new-ask))
         (workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (element (make-instance 'table-element :class :trace_box :status "" :pendingp t)))
    (setf (slot-value element 'title) (question-table new-ask-question))
    (let ((columns (length (question-seek new-ask-question))))
      (setf (slot-value element 'template) (question-column new-ask-question))
      (setf (slot-value element 'row-data) (list (tracer-make-seek new-ask new-ask-question *tracer-ask-seek-mode*)
                                                 (make-list columns :initial-element "")))
      (setf (slot-value element 'column-widths) (divide-columns *trace-element-header* columns))
      (setf (slot-value element 'body-cells) (cg:make-position columns 2))
      (setf (getf (slot-value element 'properties) 'real-title) (question-table new-ask-question)))
    (set-workspace-element-area element 0 new-ask-number)
    (install-element element graph-window)
    (setf (trace-element graph-window new-ask) element)
    (cg:redisplay-window graph-window (graph-element-extent element))
    (trace-ensure-visible graph-window (graph-element-extent element))
    (cg:select-window workspace-window)))

(defun trace-event-add-question (trace)
  (let* ((state (trace-state trace))
         (current-ask (first (state-current-asks state)))
         (current-question (first (ask-steps current-ask)))
         (workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (ask-element (trace-element graph-window current-ask))
         (ask-element-area (graph-element-area ask-element))
         (element (make-instance 'table-element :class :trace_box :pendingp t)))
    (setf (slot-value element 'title) (question-table current-question))
    (let ((columns (length (question-seek current-question))))
      (setf (slot-value element 'template) (question-column current-question))
      (setf (slot-value element 'row-data) (list (tracer-make-seek current-ask current-question *tracer-question-seek-mode*)
                                                 (make-list columns :initial-element "")))
      (setf (slot-value element 'column-widths) (divide-columns *trace-element-width* columns))
      (setf (slot-value element 'body-cells) (cg:make-position columns 2))
      (setf (getf (slot-value element 'properties) 'real-title) (question-table current-question)))
    (set-workspace-element-area element (pro:i1+ (length (rest (ask-steps current-ask)))) (ask-number current-ask))
    (install-element element graph-window)
    (let* ((status-box (tracer-get-status-box element))
           (status-element (make-instance 'status-tab-graph-element :pendingp t :status "")))
      (install-element status-element graph-window)
      (setf (graph-element-area status-element) status-box)
      (setf (getf (slot-value element 'properties) 'status-element) status-element)
      (setf (trace-element graph-window current-question) element)
      (let ((box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-extent element))))
        (nbox-union box (graph-element-extent status-element))
        (cg:redisplay-window graph-window box)
        (trace-ensure-visible graph-window box))
      (cg:select-window workspace-window))))

(defun trace-event-remove-elements (workspace-window graph-window trace elements)
  (setf (slot-value graph-window 'elements) 
          (set-difference (slot-value graph-window 'elements) elements))
  (let ((extent (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-extent (first elements)))))
    (loop for element in (rest elements)
          do (nbox-union extent (graph-element-extent element)))
    (cg:redisplay-window graph-window extent)
    (cg:select-window workspace-window)))

(defun trace-event-add-elements (workspace-window graph-window trace elements)
  (setf (slot-value graph-window 'elements) 
          (union (slot-value graph-window 'elements) elements))
  (let ((extent (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-extent (first elements)))))
    (loop for element in (rest elements)
          do (nbox-union extent (graph-element-extent element)))
    (cg:redisplay-window graph-window extent)
    (cg:select-window workspace-window)))

(defun trace-event-remove-ask (trace)
  (let* ((workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (state (trace-state trace))
         (current-ask (first (state-current-asks state))))
    (trace-event-remove-elements workspace-window graph-window trace 
      (list (trace-element graph-window current-ask)))))

(defun trace-event-remove-question (trace)
  (let* ((workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (state (trace-state trace))
         (current-ask (first (state-current-asks state)))
         (current-step (first (ask-steps current-ask)))
         (current-step-element (trace-element graph-window current-step))
         (current-step-status-element (getf (slot-value current-step-element 'properties) 'status-element)))
    (trace-event-remove-elements workspace-window graph-window trace 
      (list current-step-element current-step-status-element))))

;;; Updating a question should probably be handled more sensitively; ideally,
;;; we should miss this step out.  However, I'm not going to do this just yet. 

(defun trace-event-update-question (trace pendingp)
  (let* ((state (trace-state trace))
         (current-ask (first (state-current-asks state)))
         (current-question (first (ask-steps current-ask)))
         (workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (element (trace-element graph-window current-question))
         (status-element (getf (slot-value element 'properties) 'status-element)))
    (assert element)
    (assert status-element)
    (setf (slot-value element 'title) (question-table current-question))
    (setf (slot-value status-element 'status) (or (question-status current-question) ""))
    (setf (second (slot-value element 'row-data)) 
            (instantiate-list (ask-environment current-ask) (question-find current-question)))
    (setf (slot-value element 'pendingp) pendingp)
    (setf (slot-value status-element 'pendingp) pendingp)
    (cg:redisplay-window graph-window (cg:box-union (graph-element-extent element) (graph-element-extent status-element)))
    (trace-ensure-visible graph-window (graph-element-extent element))
    (cg:select-window workspace-window)))

(defun trace-event-completed-instruction-card (trace)
  (unless *tracer-remove-completed-instruction-cards-p*
    (return-from trace-event-completed-instruction-card))
  (let* ((state (trace-state trace))
         (workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (current-ask (first (state-current-asks state))))
    (trace-event-remove-elements workspace-window graph-window trace
      (cons (trace-element graph-window current-ask)
            (loop for step in (ask-steps current-ask)
                  for element = (trace-element graph-window step)
                  for status = (getf (slot-value element 'properties) 'status-element)
                  if element 
                    collect element
                  end
                  if status
                    collect status
                  end)))))

(defun trace-event-update-ask-seek (trace)
  (let* ((state (trace-state trace))
         (current-ask (first (state-current-asks state)))
         (current-question (ask-question current-ask))
         (workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (element (trace-element graph-window current-ask)))
    (assert element)
    (setf (first (slot-value element 'row-data))
            (tracer-make-seek current-ask current-question *tracer-ask-seek-mode*))
    (cg:redisplay-window graph-window (graph-element-extent element))
    (trace-ensure-visible graph-window (graph-element-extent element))
    (cg:select-window workspace-window)))

(defun trace-event-update-ask-find (trace pendingp)
  (let* ((state (trace-state trace))
         (current-ask (first (state-current-asks state)))
         (current-question (ask-question current-ask))
         (workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (element (trace-element graph-window current-ask)))
    (assert element)
    (setf (slot-value element 'title) (question-table current-question))
    (setf (slot-value element 'status) (if pendingp "" (question-status current-question)))
    (setf (second (slot-value element 'row-data)) (question-find current-question))
    (setf (slot-value element 'pendingp) pendingp)
    (cg:redisplay-window graph-window (graph-element-extent element))
    (trace-ensure-visible graph-window (graph-element-extent element))
    (cg:select-window workspace-window)))

