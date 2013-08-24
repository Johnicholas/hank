;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; We've got as far as the interpreter, at last. This will borrow significant
;;; elements from the new Froglet evaluator, but should be a bit simpler. The
;;; idea, however, will be basically the same. We use a state and event system,
;;; which like the command system, is generally undoable. This means the interpreter
;;; is, in principle at least, reversible. In practice, you'll need access to the
;;; right commands and to a stored set of events to run a query backwards, and that
;;; may not always be available. However, the idea is generally pretty simple. 
;;;
;;; Importantly, we need to design the interpreter so that it maps fairly cleanly
;;; onto the comic strip notation. This is not the same as a stack-based notation,
;;; although it is almost so. Each question needs to be treated separately, ideally
;;; with an identifier which increments from one. Similarly, within an answer to a 
;;; question, we have a number of steps. These two effectively construct a 2D matrix
;;; of cells, which is the comic strip. In fact, the most important thing in the
;;; state is the question. 

(defstruct (event)
  class
  subclass
  do-function
  do-data
  do-start
  undo-function
  undo-data
  undo-start
  question
  trace-event-p
  properties)

(defstruct (question)
  table
  column
  seek
  find
  status
  card
  index
  environment)

(defstruct (ask)
  question
  steps
  environment
  body
  tag
  depth
  number
  original-seek)

;;; In the state, asks is a list of all questions, in chronological order. current-asks
;;; is a list of all pending questions, again in chronological order. 

(defstruct (state)
  asks
  current-asks
  continuation
  seek
  main-window
  trace
  properties)

;;; The environment offers a trace. When we cancel a trace, by clearing the workspace,
;;; all the appropriate buttons should be reset. Similarly, the commands in the menu
;;; bar should be derived from the current trace. The trace is attached to the main
;;; window, not to the control panel or the workspace window. However, it contains
;;; pointers to both. 

(defclass trace ()
  ((at-start-p
     :initform t
     :accessor trace-at-start-p)
   (at-end-p
     :initform nil
     :accessor trace-at-end-p)
   (completed-p
     :initform nil
     :accessor trace-completed-p)
   (state
     :initarg :state
     :reader trace-state)
   (restart
     :initform :start
     :accessor trace-restart)
   (control-panel-window
     :initarg :control-panel-window
     :reader trace-control-panel-window)
   (workspace-window
     :initarg :workspace-window
     :reader trace-workspace-window)
   (main-window
     :initarg :main-window
     :reader trace-main-window)))

;;; A workspace trace is a bit more complicated than a standard trace, but the
;;; ideas are fairly simple. The idea is simply that we should, live, be able to
;;; run bundles of events backwards and forwards, although mainly this will be
;;; forwards. 

(defclass workspace-trace (trace)
  ((event-buffer
     :initform nil
     :accessor workspace-trace-event-buffer)))

(defun get-special-window (main-window window-name)
  (or (cg:find-named-object window-name (main-window-body main-window))
      (let* ((menu-bar (cg:window-menu main-window))
             (menu-item (cg:find-named-object window-name menu-bar)))
        (assert menu-item)
        (funcall (cg:menu-item-value menu-item) main-window :window-state :shrunk))))

(defun get-workspace-window (main-window)
  (get-special-window main-window :workspace-window))
(defun get-control-panel-window (main-window)
  (get-special-window main-window :control-panel-window))

(comtab:defmenu-command :step ((window main-window))
  (record-command window "Step")
  (let ((trace (main-window-trace window)))
    (when trace 
      (ask-step trace))))

(comtab:defmenu-command :step-back ((window main-window))
  (record-command window "Step Back")
  (let ((trace (main-window-trace window)))
    (when trace 
      (ask-step-back trace))))
     
(comtab:defmenu-command :run ((window main-window))
  (record-command window "Run")
  (let ((trace (main-window-trace window)))
    (when trace 
      (ask-step trace)
      (start-background-timer #'ask-step window))))

(comtab:defmenu-command :run-back ((window main-window))
  (record-command window "Run Back")
  (let ((trace (main-window-trace window)))
    (when trace 
      (ask-step-back trace)
      (start-background-timer #'ask-step-back window))))

(defun ask-step-back (trace)
  (when (typep trace 'workspace-trace)
    (let ((events (pop (workspace-trace-event-buffer trace))))
      (when events
        (setf (trace-at-end-p trace) nil)
        (setf (trace-completed-p trace) nil)
        (loop with restart = (trace-restart trace)
              with state = (trace-state trace)
              for event in events 
              do (progn
                   (when (event-undo-function event) (funcall (event-undo-function event) trace state event))
                   (setf restart (event-undo-start event)))
              finally do (setf (trace-restart trace) restart))
        (let ((widget (cg:get-stream-prop (trace-workspace-window trace) 'step-number)))
          (decf (cg:dialog-item-value widget))))
      (unless events
        (stop-background-timer))
      (enable-control-panel-buttons trace
        `(:step :run ,@(when (workspace-trace-event-buffer trace)
                               '(:step-back :run-back)))))))

;;; Stepping is easy. Running is a bit harder. To run, we should start a background
;;; timer on the window which will continually step until the end of the execution.
;;; While we do this, we can switch to the waiting cursor. The interface can, of
;;; course, still be active during this time. 

(defconstant *background-timer-id* 8)
(defvar *background-timer-widget* ())
(defvar *background-timer-function* ())

(defconstant *process-tick-interval* 50)

(defun start-background-timer (function widget)
  (stop-background-timer)
  (when pc::*timer-hwnd*
    (when (acl:izerop (win::SetTimer pc::*timer-hwnd* *background-timer-id* *process-tick-interval* ct:hnull))
      #-Runtime-System
      (cg:lisp-message "Couldn't create background timer")
      nil)
    (setf *background-timer-widget* widget)
    (setf *background-timer-function* function)))

(defun stop-background-timer-exit-fn (everything)
  (declare (ignore everything))
  (stop-background-timer))
(pushnew 'stop-background-timer-exit-fn acl::*system-exit-fns*)

(defun stop-background-timer ()
  (setf *background-timer-widget* nil)
  (setf *background-timer-function* nil)
  (when pc::*timer-hwnd*
    (win::KillTimer pc::*timer-hwnd* *background-timer-id*)))

(defmethod pc::window-procedure (window 
                                 (msg (eql win::wm_timer))
                                 (wparam (eql *background-timer-id*))
                                 lparam)
  (background-timer-tick *background-timer-function* *background-timer-widget*))

(defun background-timer-tick (function window)
  (let ((trace (when (typep window 'main-window) (main-window-trace window))))
    (if (and trace (not (trace-completed-p trace)))
        (funcall function trace)
        (stop-background-timer))))

(defun (setf main-window-trace) (value main-window)
  (let ((trace (cg:get-stream-prop main-window 'trace)))
    (when trace
      (unless (trace-at-end-p trace)
        (trace-complete trace))
      (setf trace nil))
    (cg:set-stream-prop main-window 'trace value)))

(defun main-window-trace (main-window)
  (cg:get-stream-prop main-window 'trace))

(defun get-question (main-window)
  (let* ((control-panel-window (get-control-panel-window main-window))
         (input-window (cg:get-stream-prop control-panel-window 'input-window)))
    (complete-element-editor input-window :end)
    (first (convert-file-elements-to-interpreter-elements 
             (loop for (element graph-element) in (get-file-elements (graph-elements input-window))
                   collect element)))))

(defun make-ask-state (main-window &aux state)
  (let* ((value (get-question main-window))
         (question (make-question :table (compound-operator value)
                                  :column (third (compound-arguments value))
                                  :seek (fourth (compound-arguments value))))
         (ask (make-ask :question question :environment (list (cons *status-key* "OK")) :depth 0 :number 0)))
    (make-state :asks (list ask) :current-asks (list ask) 
      :seek (question-seek question)
      :main-window main-window)))

;;; This is where we should be dealing with questions being cancelled. At 
;;; least, that's my view. 

(defmethod trace-store ((trace trace) main-window)
  (let ((trace (main-window-trace main-window)))
    (when trace (trace-complete trace)))
  (:clear-workspace (get-workspace-window main-window))
  (setf (main-window-trace main-window) trace))

(defun make-tracer-from-current-question (main-window class &rest options)
  (let* ((state (make-ask-state main-window))
         (trace (apply #'make-instance class
                  :state state
                  :control-panel-window (get-control-panel-window main-window)
                  :main-window main-window
                  :workspace-window (get-workspace-window main-window)
                  options)))
    (setf (state-trace state) trace)
    (setf (state-main-window state) main-window)
    (trace-store trace main-window)
    (trace-begin trace)
    trace))

(comtab:defmenu-command :ask-briefly ((window control-panel-window))
  (let ((main-window (application-window-main-window window)))
    (record-command main-window "Ask Briefly")
    (ask-step (make-tracer-from-current-question main-window 'trace)))
  t)
 
(comtab:defmenu-command :ask-in-full ((window control-panel-window))
  (let ((main-window (application-window-main-window window)))
    (record-command main-window "Ask In Full")
    (make-tracer-from-current-question main-window 'workspace-trace))
  t)

(comtab:defmenu-command :ask-briefly ((window card-editor-window))
  (let ((main-window (application-window-main-window window)))
    (record-command main-window "Put Question In Control Panel")
    (ask-install-question window)
    (:ask-briefly (get-control-panel-window main-window))))

(comtab:defmenu-command :put-question-in-control-panel ((window card-editor-window))
  (ask-install-question window))

(comtab:defmenu-command :ask-in-full ((window card-editor-window))
  (let ((main-window (application-window-main-window window)))
    (record-command main-window "Put Question In Control Panel")
    (ask-install-question window)
    (:ask-in-full (get-control-panel-window main-window))))

(comtab:defmenu-command :ask-briefly ((window main-window))
  (or (call-next-method)
      (cg:select-window (get-control-panel-window window))))

(comtab:defmenu-command :ask-in-full ((window main-window))
  (or (call-next-method)
      (cg:select-window (get-control-panel-window window))))

(comtab:defmenu-command :clear-workspace ((window main-window))
  (record-command window "Clear Workspace")
  (let ((workspace-window (get-workspace-window window)))
    (:clear-workspace workspace-window)))

(defmethod trace-begin ((trace trace))
  (let* ((state (trace-state trace))
         (ask (first (state-current-asks state)))
         (question (ask-question ask))
         (output-window (cg:get-stream-prop (trace-control-panel-window trace) 'output-window)))
    (file-position output-window :end)
    (format output-window
      "You ask ~A: \"~A with ~{~@[~A is ~]~A~^, ~}\"~%"
      *character-name*
      (question-table question)
      (loop for column in (question-column question)
            for value in (question-seek question)
            collect (unless (zerop (length column)) column)
            collect value))))

(defun enable-control-panel-buttons (trace buttons)
  (let ((control-window (cg:get-stream-prop (trace-workspace-window trace) 'control-window)))
    (loop for item in (cg:dialog-items control-window)
          when (typep item 'cg:pixmap-button)
          do (setf (cg:dialog-item-available-p item) (member (cg:object-name item) buttons)))))

(defmethod trace-begin ((trace workspace-trace))
  (call-next-method)
  (let ((control-window (cg:get-stream-prop (trace-workspace-window trace) 'control-window)))
    (:clear-workspace (trace-main-window trace))
    (enable-control-panel-buttons trace '(:step :run))
    (trace-event-add-ask trace)))

;;; This is the ask loop that needs to be unfolded for stepping. The idea is pretty
;;; simple really, we simply add a criterion for the execution to end, and the restart
;;; is taken from the window to begin with. Only if we end at the end of the execution
;;; is the event nil, and we mark that with the restart :end. 

(defmethod ask-stop-event-p ((trace trace) event)
  nil)

(defmethod ask-stop-event-p ((trace workspace-trace) event)
  (event-trace-event-p event))

(defmethod ask-get-next-event ((trace trace) state restart)
  (%%execute state restart))

(defmethod ask-step ((trace trace))
  (when (trace-at-end-p trace)
    (return-from ask-step t))
  (loop with restart = (trace-restart trace)
        with state = (trace-state trace)
        with event
        do (progn
             (unless state
               (return nil))
             (setf event (ask-get-next-event trace state restart))
             (unless event
               (trace-end trace)
               (return nil))
             (when (event-do-function event) (funcall (event-do-function event) trace state event))
             (setf restart (event-do-start event))
             (when (ask-stop-event-p trace event)
               (setf (trace-restart trace) restart)
               (loop-finish))
             (cg:process-single-event t))
        finally do (return t))
  (when (trace-at-end-p trace)
    (trace-complete trace)))

(defmethod ask-step ((trace workspace-trace))
  (let ((*events* ()))
    (declare (special *events*))
    (prog1
      (call-next-method)
      (push *events* (workspace-trace-event-buffer trace))
      (let ((widget (cg:get-stream-prop (trace-workspace-window trace) 'step-number)))
        (incf (cg:dialog-item-value widget)))
      (enable-control-panel-buttons trace
        `(:step-back :run-back ,@(unless (trace-at-end-p trace) '(:step :run)))))))

(defmethod ask-get-next-event ((trace workspace-trace) state restart)
  (let ((event (call-next-method)))
    (declare (special *events*))
    (when event
      (push event *events*))
    event))

(defmethod trace-end ((trace trace))
  (setf (trace-at-end-p trace) t))

(defmethod trace-complete ((trace trace))
  (when (trace-completed-p trace)
    (return-from trace-complete))
  (setf (trace-completed-p trace) t)
  (let* ((state (trace-state trace))
         (answer (first (state-current-asks state)))
         (question (ask-question answer))
         (environment (ask-environment answer))
         (status (question-status question))
         (output-window (cg:get-stream-prop (trace-control-panel-window trace) 'output-window)))
    (file-position output-window :end)
    (unless (trace-at-end-p trace)
      (format output-window "Question cancelled~%~%")
      (return-from trace-complete))
    (if (pro:whole-string-equal status "Fail")
        (setf environment (list (cons *status-key* "Fail")))
        (progn
          (setf environment (list (first environment)))
          (setf environment (match (state-seek state) (question-find question) environment))))
    (format output-window "Status = ~A~%~{~A = ~A~%~}~%"
      status
      (let ((environment (remove *status-key* environment :key #'first)))
        (setf environment (sort environment #'pro:whole-string-lessp :key #'first))
        (loop for pair in environment
              collect (first pair)
              collect (rest pair))))))

;;; This method copies the passed element of a card editor into the control 
;;; panel window. It is used when we click on Hank: Ask special tables to
;;; copy the information into the control panel. It will usually then be
;;; followed by a command, such as the :ask-briefly command. 

(defmethod complete-ask-question ((window card-editor-window) element)
  (let* ((control-panel-window (get-control-panel-window
                                 (application-window-main-window window)))
         (input-window (cg:get-stream-prop control-panel-window 'input-window))
         (question-element (first (graph-elements input-window))))
    (assert question-element)
    (setf (slot-value question-element 'body-cells) (cg:copy-position (table-element-body-cells element)))
    (setf (slot-value question-element 'title) (table-element-title element))
    (setf (slot-value question-element 'row-data) (copy-tree (table-element-row-data element)))
    (setf (slot-value question-element 'template) (copy-list (table-element-template element)))
    (setf (slot-value question-element 'column-widths) (copy-list (table-element-column-widths element)))
    (setf (slot-value question-element 'area) (cg:copy-box (slot-value question-element 'area)))
    (let ((area (calculate-table-element-area question-element)))
      (setf (graph-element-area question-element t)
              (cg:make-box *question-box-space* *question-box-space*
                (pro:i+ *question-box-space* (cg:box-width area)) 
                (pro:i+ *question-box-space* (cg:box-height area)))))
    (cg:invalidate-window input-window)
    (cg:select-window control-panel-window)))

