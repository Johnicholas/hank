;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; Handle events for the interpreter, with and without tracing. Events should appear as
;;; the primitives of the reversible execution model. Generally, new events should be 
;;; represented on the comic strip workspace window if switched on. We need a few parameters
;;; to work with the columns and all that sort of thing. 
;;;
;;; Some additional stuff is needed now. We want to be able to drive the tracer in a 
;;; step by step manner from the interface. This means that we should also provide
;;; feedback about the current state of execution, and the restart, and that sort of
;;; thing. The idea is simple: the workspace window holds a restart, if there is one, 
;;; and if there isn't, then that's that. We'll worry about the reversible part of
;;; the execution model later, but this generally means we need to remember a bunch
;;; of events in one go. 
;;; 
;;; We should handle this a bit differently, so we can print the workspace properly. The
;;; idea is that the area is calculated from two points, and x coordinate and a y coordinate.
;;; These will map into a box, and they should be saved as part of the element. Then they
;;; can be used to reconstruct the box after it has been moved to the printer window. That
;;; deals with the mismatch between the storyboard headers and the elements on the
;;; storyboard itself. If x is zero, then this is a question, otherwise it is an ask. 

(defmethod event-do-special ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-environment (ask-environment current-ask))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (destructuring-bind (function columns seek) (event-do-data event)
      (setf (event-undo-data event) current-environment)
      (let ((new-environment (funcall function state event current-environment columns seek)))
        (setf (question-status current-question) (rest (assoc *status-key* new-environment)))
        (setf (question-find current-question) (question-seek current-question))
        (setf (question-environment current-question) new-environment)
        (setf (ask-environment current-ask) new-environment)))))

(defmethod event-do-special ((trace workspace-trace) state event)
  (call-next-method)
  (setf (event-trace-event-p event) t)
  (if (ask-steps (first (state-current-asks state)))
      (trace-event-update-question trace nil)
      (trace-event-update-ask-find trace nil)))

;;; Specials may have something else which needs to be run, if the event has a side effect
;;; which needs to be handled when the trace event is undone.  

(defmethod event-undo-special ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (setf (question-status current-question) nil)
    (setf (question-find current-question) nil)
    (setf (ask-environment current-ask) (event-undo-data event))
    (setf (event-undo-data event) nil)
    (let ((command (getf (event-properties event) 'undo-command)))
      (when command
        (setf (getf (event-properties event) 'undo-command) nil)
        (command-undoit command)))))

(defmethod event-undo-special ((trace workspace-trace) state event)
  (call-next-method)
  (let ((current-ask (first (state-current-asks state))))
    (if (ask-steps current-ask)
        (trace-event-update-question trace t)
        (trace-event-update-ask-find trace t))))

(defmethod event-do-special-card ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (destructuring-bind (card) (event-do-data event)
      (setf (question-card current-question) card))))

(defmethod event-undo-special-card ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (setf (question-card current-question) nil)))

;;; The user card event is not being mapped into a tracer event. We should really
;;; do this, so we can map between the original question and the new question
;;; in the given card. In fact, we only really need to take special care with
;;; this when the user card is an instruction card. 

(defmethod event-do-user-card ((trace workspace-trace) state event)
  (let ((current-ask (first (state-current-asks state))))
    (unless (ask-steps current-ask)
      (setf (ask-original-seek current-ask) (question-seek (ask-question current-ask))))
    (call-next-method)
    (unless (ask-steps current-ask)
      (setf (event-trace-event-p event) t)
      (trace-event-update-ask-seek trace))))

(defmethod event-do-user-card ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (destructuring-bind (card) (event-do-data event)
      (setf (question-card current-question) card)
      (setf (event-undo-data event)
              (list (question-column current-question) (question-seek current-question)))
      
      (when (eq current-question (ask-question current-ask))
        (let ((value (compound-arguments card)))
          (when (pro:whole-string-equal (first value) "Instruction_Card")
            (setf (question-column current-question) (third value))
            (setf (question-seek current-question)
                    (loop for element in (question-seek current-question)
                          for pattern in (fourth value)
                          collect (if (variablep element) 
                                      pattern 
                                      element)))))))))

(defmethod event-undo-user-card ((trace workspace-trace) state event)
  (let ((current-ask (first (state-current-asks state))))
    (call-next-method)
    (unless (ask-steps current-ask)
      (setf (ask-original-seek current-ask) nil)
      (trace-event-update-ask-seek trace))))

(defmethod event-undo-user-card ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (setf (question-card current-question) nil)
    (destructuring-bind (undo-column undo-seek) (event-undo-data event)
      (setf (question-column current-question) undo-column)
      (setf (question-seek current-question) undo-seek))))

(defmethod event-do-fact ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-environment (ask-environment current-ask))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (destructuring-bind (entry new-environment) (event-do-data event)
      (setf (question-status current-question) "OK")
      (setf (question-find current-question) (question-seek current-question))
      (setf (event-undo-data event) (ask-environment current-ask))
      (setf (ask-environment current-ask) (acons *status-key* "OK" (rest new-environment)))
      (setf (question-environment current-question) (ask-environment current-ask)))))

(defmethod event-do-fact ((trace workspace-trace) state event)
  (call-next-method)
  (setf (event-trace-event-p event) t)
  (trace-event-update-question trace nil))

(defmethod event-undo-fact ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (setf (question-status current-question) nil)
    (setf (question-find current-question) nil)
    (setf (ask-environment current-ask) (event-undo-data event))))

(defmethod event-undo-fact ((trace workspace-trace) state event)
  (call-next-method)
  (trace-event-update-question trace t))

(defmethod event-do-answer-fail ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-environment (ask-environment current-ask))
         (new-environment (acons *status-key* "Fail" (rest current-environment)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (setf (question-status current-question) "Fail")
    (setf (event-undo-data event) (ask-environment current-ask))
    (setf (ask-environment current-ask) new-environment)
    (setf (question-environment current-question) (ask-environment current-ask))))

(defmethod event-do-answer-fail ((trace workspace-trace) state event)
  (call-next-method)
  (setf (event-trace-event-p event) t)
  (let ((current-ask (first (state-current-asks state))))
    (if (ask-steps current-ask)
        (trace-event-update-question trace nil)
        (trace-event-update-ask-find trace nil))))

(defmethod event-undo-answer-fail ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (setf (question-status current-question) nil)
    (setf (ask-environment current-ask) (event-undo-data event))))

(defmethod event-undo-answer-fail ((trace workspace-trace) state event)
  (call-next-method)
  (let ((current-ask (first (state-current-asks state))))
    (if (ask-steps current-ask)
        (trace-event-update-question trace t)
        (trace-event-update-ask-find trace t))))

;;; Start an instruction card, by finding the card and writing its first
;;; question in the first steps. First, though, put the card steps and
;;; first tag into the current ask. 

(defmethod event-do-instruction ((trace trace) state event)
  (destructuring-bind (instruction-card environment) (event-do-data event)
    (let ((current-ask (first (state-current-asks state))))
      (setf (ask-body current-ask) (nthcdr 4 (compound-arguments instruction-card)))
      (setf (event-undo-data event) (ask-environment current-ask))
      (setf (ask-tag current-ask) (first (ask-body current-ask)))
      (let ((question (first (rest (member (ask-tag current-ask) (ask-body current-ask)
                                     :test #'(lambda (value1 value2)
                                               (and (stringp value2)
                                                    (pro:whole-string-equal value1 value2))))))))
        (assert question ()
          'hank-error
          :format-control "The body of an instruction card must contain something!")
        (push (make-question :table (compound-operator question)
                             :column (third (compound-arguments question))
                             :seek (fourth (compound-arguments question))
                             :index 1
                             :environment environment)
          (ask-steps current-ask)))

      (setf (ask-environment current-ask) environment))))

(defmethod event-do-instruction ((trace workspace-trace) state event)
  (call-next-method)
  (setf (event-trace-event-p event) t)
  (trace-event-add-question trace))

(defmethod event-undo-instruction ((trace trace) state event)
  (let ((current-ask (first (state-current-asks state))))
    (setf (ask-body current-ask) nil)
    (setf (ask-tag current-ask) nil)
    (pop (ask-steps current-ask))
    (setf (ask-environment current-ask) (event-undo-data event))))

(defmethod event-undo-instruction ((trace workspace-trace) state event)
  (trace-event-remove-question trace)
  (call-next-method))

;;; It is here that the more cunning execution model needs to be added.  Basically,
;;; doing a step may involve popping steps, and undoing it may involve adding them
;;; again.  All this also needs to link to the tracer, and we need to properly
;;; support the value box binding model.  Mainly, we need to be a bit more careful 
;;; with the environment, so that variables can be handled properly. 
;;;
;;; Basically, we need to be able to dive back to an earlier environment, and keep 
;;; all its variables, with the later values.  We also need to be able to undo this
;;; after we've done it.  We also need to be able to detect when we're going back to
;;; a previous step in an instruction card.  The index, which starts at 1, is the 
;;; number of the question step in the instruction card body.  If we ever come to
;;; the same number as a previous step, we can drop everything between the two off
;;; the steps, but we also (a) need to make this undoable, and (b) set the environment
;;; properly.  

(defmethod event-do-instruction-step ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (save-ask-tag (ask-tag current-ask))
         (save-environment (ask-environment current-ask))
         (save-steps (ask-steps current-ask)))
    (setf (ask-tag current-ask) (first (event-do-data event)))
    (loop with index = 1
          with tag = (ask-tag current-ask)
          for elements = (ask-body current-ask) then (rest elements)
          for element = (first elements)
          do (cond ((null element) (return))
                   ((compound-p element) (pro:iincf index))
                   ((stringp element)
                    (when (pro:whole-string-equal element tag)
                      (let* ((question-form (first (rest elements)))
                             (question (make-question :table (compound-operator question-form)
                                                      :column (third (compound-arguments question-form))
                                                      :seek (fourth (compound-arguments question-form))
                                                      :index index)))
                        (let ((previous-step (member index (ask-steps current-ask) :key #'question-index)))
                          (if (null previous-step)
                              (push question (ask-steps current-ask))
                              (let* ((old-environment (rest (question-environment (first previous-step))))
                                     (environment (ask-environment current-ask))
                                     (new-environment 
                                       (cons (first environment)
                                             (loop for binding in old-environment
                                                   collect (cons (first binding)
                                                           (rest (assoc (first binding) (rest environment)
                                                                   :test #'pro:whole-string-equal)))))))
                                (setf (ask-environment current-ask) new-environment)
                                (setf (ask-steps current-ask)
                                        (cons question (rest previous-step)))))))))))
    (setf (event-undo-data event) (list save-ask-tag save-environment save-steps))))

(defmethod event-undo-instruction-step ((trace trace) state event)
  (let ((current-ask (first (state-current-asks state))))
    (destructuring-bind (save-ask-tag save-environment save-steps) (event-undo-data event)
      (setf (ask-tag current-ask) save-ask-tag)
      (setf (ask-environment current-ask) save-environment)
      (setf (ask-steps current-ask) save-steps))))

;;; When we are popping steps off the stack, we want to reflect that by removing some stuff from 
;;; the tracer window.  We can do this simply by counting the steps before and after, and removing
;;; any questions that ought to be removed.  
       
(defmethod event-do-instruction-step ((trace workspace-trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (ask-steps-before (ask-steps current-ask)))
    (call-next-method)
    (let* ((ask-steps-after (ask-steps current-ask))
           (before (length ask-steps-before))
           (after (length ask-steps-after)))
      
      ;; If there were more steps before than after (or the same number) then remove
      ;; the appropriate elements
       
      (when (> before after)
        (let* ((workspace-window (trace-workspace-window trace))
               (graph-window (cg:get-stream-prop workspace-window 'output-window))
               (elements (loop for element in ask-steps-before
                               for index from after to before
                               for graph-element = (trace-element graph-window element)
                               for status-element = (getf (slot-value graph-element 'properties) 'status-element)
                               collect graph-element
                               if status-element
                                 collect status-element)))
          (trace-event-remove-elements workspace-window graph-window trace elements)
          (setf (getf (event-properties event) 'trace-undo-data) elements))))
    (setf (event-trace-event-p event) t)
    (trace-event-add-question trace)))

(defmethod event-undo-instruction-step ((trace workspace-trace) state event)
  (trace-event-remove-question trace)
  (call-next-method)
  (let* ((workspace-window (trace-workspace-window trace))
         (graph-window (cg:get-stream-prop workspace-window 'output-window))
         (elements (getf (event-properties event) 'trace-undo-data)))
    (when elements
      (trace-event-add-elements workspace-window graph-window trace elements))
    (remf (event-properties event) 'trace-undo-data)))

(defmethod event-do-complete ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (ask-question current-ask))
         (last-step (or (first (ask-steps current-ask)) (ask-question current-ask))))
    (unless (pro:whole-string-equal "Fail" (setf (question-status current-question) (question-status last-step)))
      (setf (event-undo-data event) (question-find current-question))
      (setf (question-find current-question) (instantiate-list
                                               (ask-environment current-ask)
                                               (question-seek current-question))))))

;;; The event only suspends the trace if there is another question yet to be
;;; answered. This means that this step, for the first line, ends and completes
;;; the trace properly. 

(defmethod event-do-complete ((trace workspace-trace) state event)
  (call-next-method)
  (setf (event-trace-event-p event) (rest (state-current-asks state)))
  (trace-event-update-ask-find trace nil))

(defmethod event-undo-complete ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (ask-question current-ask)))
    (setf (question-find current-question) (event-undo-data event))))

(defmethod event-undo-complete ((trace workspace-trace) state event)
  (call-next-method)
  (trace-event-update-ask-find trace t))

;;; For sanity, we should *not* import wildcards into the inside of an instruction
;;; card.  This means we should make sure wildcards have the right names in the
;;; seek part of the card, and in the initial environment. 

(defmethod event-do-ask ((trace trace) state event)
  (destructuring-bind (instruction-card environment) (event-do-data event)
    (let* ((current-ask (first (state-current-asks state)))
           (current-question (or (first (ask-steps current-ask)) (ask-question current-ask)))
           (current-environment (ask-environment current-ask))
           (new-column (instantiate-list current-environment (question-column current-question)))
           (new-seek (instantiate-list current-environment (question-seek current-question)))
           (new-ask (make-ask 
                      :depth (1+ (ask-depth current-ask))
                      :number (1+ (ask-number (first (state-asks state)))))))
      (setf (ask-environment new-ask) environment)
      (setf (ask-question new-ask) 
              (make-question :table (question-table current-question)
                             :column new-column
                             :seek new-seek
                             :card (question-card current-question)))
      (push new-ask (state-asks state))
      (push new-ask (state-current-asks state)))))
      
(defmethod event-do-ask ((trace workspace-trace) state event)
  (call-next-method)
  (setf (event-trace-event-p event) t)
  (trace-event-add-ask trace))

(defmethod event-undo-ask ((trace trace) state event)
  (pop (state-current-asks state))
  (pop (state-asks state)))

(defmethod event-undo-ask ((trace workspace-trace) state event)
  (trace-event-remove-ask trace)
  (call-next-method))

;;; In a nonlinear trace, as we pop the current asks, we also remove the ask and all
;;; its questions. This is what makes it different from a linear trace. We decide this
;;; when we get to this point, so that people can switch between the two at more or
;;; less any time. Also, a nonlinear trace may erase failed steps, because they do
;;; not change any bindings. We probably need a more complex tracer structure, which
;;; can handle a lot of this stuff. 

(defmethod event-do-instruction-return ((trace trace) state event)
  (let* ((current-ask (first (state-current-asks state)))
         (return-ask (first (rest (state-current-asks state))))
         (current-question (ask-question current-ask))
         (return-question (or (first (ask-steps return-ask)) (ask-question return-ask))))
    (setf (event-undo-data event)
            (list current-ask (question-status return-question) 
                  (question-find return-question) (ask-environment return-ask)))
    (setf (question-status return-question) (question-status current-question))
    (if (pro:whole-string-equal (question-status return-question) "Fail")
        (setf (ask-environment return-ask) (acons *status-key* "Fail" (rest (ask-environment return-ask))))
        (let ((new-environment (ask-environment return-ask)))
          (setf new-environment (match (question-seek return-question)
                                       (instantiate-list (ask-environment current-ask)
                                         (question-find current-question))
                                  new-environment))
          (assert (not (eq new-environment :fail)))
          (setf (question-find return-question) (question-seek return-question))
          (setf (ask-environment return-ask) new-environment)))
    (pop (state-current-asks state))))

(defmethod event-do-instruction-return ((trace workspace-trace) state event)
  (trace-event-completed-instruction-card trace)
  (call-next-method)
  (setf (event-trace-event-p event) t)
  (trace-event-update-question trace nil))

(defmethod event-undo-instruction-return ((trace trace) state event)
  (destructuring-bind (old-current-ask old-return-status old-return-find old-return-environment)
                      (event-undo-data event)
    (let* ((return-ask (first (state-current-asks state)))
           (return-question (or (first (ask-steps return-ask)) (ask-question return-ask))))
      (setf (question-status return-question) old-return-status)
      (setf (question-find return-question) old-return-find)
      (setf (ask-environment return-ask) old-return-environment)
      (push old-current-ask (state-current-asks state)))))

;;; Need to be more cunning here. Update the right question, which is not necessarily
;;; the most recent question. Fake this carefully. 

(defmethod event-undo-instruction-return ((trace workspace-trace) state event)
  (call-next-method)
  (let ((current-ask (pop (state-current-asks state))))
    (unwind-protect (trace-event-update-question trace t)
      (push current-ask (state-current-asks state)))))

;;; It would be great if this was linked to the rest of the system, so that when printing
;;; only the right number of lines will be printed. This doesn't matter when drawing, 
;;; because an infinite space is OK. But when we're printing, we don't really want an
;;; infinite number of comic strip sheets. 

(defun redraw-workspace-grid (window box)
  (macrolet ((move (x y)
               `(cg:move-to window (cg:nmake-position '#.(cg:make-position 0 0) ,x ,y)))
             (draw (x y)
               `(cg:draw-to window (cg:nmake-position '#.(cg:make-position 0 0) ,x ,y))))
  
    (cg:set-foreground-color window *grid-colour*)
  
    ;; Draw the grid, horizontal lines first, then vertical ones. The parameters should all be 
    ;; derived from the tracer box sizes. try not to draw anything unnecessarily, because we'll
    ;; be calling this function a lot. When we're drawing the horizontal lines, we can also 
    ;; draw the necessary text labels. 
   
    (let* ((line-space (pro:i+ *trace-element-row-space* (trace-element-height)))
           (top-line (pro:i/ (pro:i+ (cg:box-top box) (pro:i1- line-space)) line-space))
           (bottom-line (pro:i/ (cg:box-bottom box) line-space))
           (inset (pro:i- *trace-element-inset* 2)))
      (loop for i from top-line to bottom-line
            for y = (pro:i* i line-space)
            do (move (cg:box-left box) y)
               (draw (cg:box-right box) y))
       
      (when (and (pro:i< inset (cg:box-right box)) (pro:i<= (cg:box-left box) inset))
        (move inset (cg:box-top box))
        (draw inset (cg:box-bottom box)))
       
      (cg:set-foreground-color window *title-colour*)
      (cg:set-font window *table-title-font*)
      
      (loop for i from (pro:i1- top-line) to bottom-line
            for y = (pro:i+ (pro:i* i line-space) *trace-element-row-space*)
            do (move 5 y)
               (format window "~D." (pro:i1+ i))
            do (loop for string in '("Card" "Column" "Seek" "Find")
                     for compensation in '(0 #.*table-border-size* 0 0)
                     do (move *trace-header-inset* y)
                        (write-string string window)
                        (pro:iincf y (pro:i+ *grid-row-height* compensation)))))))
