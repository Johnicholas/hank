;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: HANK; Base: 10 -*-
;;;
;;; CONDITIONS
;;;
;;; A nonportable debugger, which replaces that in the standard conditions
;;; sample implementation. This should make use of Common Graphics and
;;; will be delivered in the runtime Ousel workbench. Runtime errors
;;; in the workbench should, as far as possible, be caught here. We will
;;; also connect the standard error handling in Allegro/Procyon so that
;;; it appears through this interface. 

(in-package "HANK")

(defvar *debug-abort* nil)
(defvar *debug-continue* nil)
(defvar *debug-condition* nil)
(defvar *debug-restarts* nil)
(defvar *number-of-debug-restarts* 0)
(defvar *expanded-restart-label-p* nil)

;;; Debugger commands are not really right as textual things. We will
;;; map them to buttons in a dialog. There will also be a list of 
;;; something, probably the pending restarts. 

(defun get-restart-label (restart)
  (with-output-to-string (stream)
    (when (eq restart *debug-abort*) (write-string "(Stop) " stream))
    (when (eq restart *debug-continue*) (write-string "(Continue) " stream))
    (write restart :stream stream :escape nil)))

;;; Debugger dialogs are constructed on demand. We still use
;;; a special variable to show the scope of the dialog, but many
;;; may be active simultaneously, although only one needs to be
;;; displayed at a time. 

(defvar *debugger-window* ())

(defun make-debugger-window ()
  (declare (special *top-level-window*))
  (cg:open-dialog (list (cg:make-dialog-item :widget 'cg:default-button
                                             :box #.(cg:make-box 175 3 215 13)
                                             :dialog-units-p t
                                             :name :restart
                                             :title "Continue"
                                             :set-value-fn 'handle-debugger-restart)
                        #||
                        (cg:make-dialog-item :widget 'cg:button
                                             :box #.(cg:make-box 175 25 215 35)
                                             :dialog-units-p t
                                             :name :continue
                                             :title "Continue"
                                             :set-value-fn 'handle-debugger-continue)
                        (cg:make-dialog-item :widget 'cg:button
                                             :box #.(cg:make-box 175 37 215 47)
                                             :dialog-units-p t
                                             :name :stop
                                             :title "Stop"
                                             :set-value-fn 'handle-debugger-stop)
                        ||#
                        (cg:make-dialog-item :widget 'cg:single-item-list 
                                             :box #.(cg:make-box 2 2 160 75)
                                             :dialog-units-p t
                                             :border :black
                                             :key #'get-restart-label
                                             :set-value-fn 'handle-debugger-select
                                             :double-click-fn #'(lambda (dialog widget)
                                                                  (handle-debugger-restart widget t nil))
                                             :name :restarts)
                        (cg:make-dialog-item :widget 'cg:static-text
                                             :box #.(cg:make-box 2 78 215 98)
                                             :dialog-units-p t
                                             :name :message)
                        (cg:make-dialog-item :widget 'cg:static-text
                                             :box #.(cg:make-box 2 102 215 116)
                                             :dialog-units-p t
                                             :name :restart-message))
    'cg:dialog *top-level-window*
    :font *message-font*
    :pop-up-p t
    :title "Error"
    :dialog-units-p t
    :visible-box (cg:make-box 0 0 218 120)
    :user-closable nil
    :window-state :shrunk))

(defun show-restarts (dialog &optional (restarts *debug-restarts*))
  (cg:set-dialog-item-range (cg:find-named-object :restarts dialog) restarts))

;;; While the dialog is up, we should actually go into a new
;;; toploop. This means that the dialog should be modeless, and
;;; that we have to take greater care of it when it goes away 
;;; or when we shift from one level to another. 
;;;
;;; When we quit the debugger we need to hide the dialog window,
;;; and if we are returning to an outer debugger, we need to fill
;;; the dialog again. 

(defun update-debugger-window (window condition)
  (let* ((restarts (cg:find-named-object :restarts window)))
    (cg:set-dialog-item-range restarts *debug-restarts*)
    (cg:set-dialog-item-value restarts (or *debug-continue* *debug-abort*))
    (cg:set-dialog-item-available-p (cg:find-named-object :restart window) *debug-restarts*)
    (cg:set-dialog-item-value (cg:find-named-object :message window)
      (write-to-string condition :escape nil))
    (cg:set-stream-title window 
      (with-output-to-string (stream)
        (format stream "Debugger")))
    window))

;;; It is around here that we should be able to handle the condition
;;; system links to quit this level of toploop and return to an outer
;;; one. In effect, add a restart here that quits this toploop. However,
;;; we also need to handle the possibility that an outer toploop might
;;; quit here. When it does, we need to again call update-debugger-dialog
;;; and select-window to ensure that we've filled out the dialog contents
;;; correctly for this level. 

#+Runtime-System
(setq *debugger-hook* #'(lambda (condition value)
                          (nonstandard-debugger condition)))

;;; A bit of a hack, but a useful one. A restart filter for our debugger. 
;;; This allows us to remove restarts from the debugger when they can cause
;;; trouble. If any filter function returns t, the restart will be removed 
;;; from the debugger. 

(defun compute-filtered-restarts (condition &aux names)
  (loop for restart in (compute-restarts condition)
        if (not (find (restart-name restart) names))
          collect restart
          and do (push (restart-name restart) names)
        end))

(defun find-named-restart (identifier restarts)
  (loop for restart in restarts
        if (eql identifier (restart-name restart))
          do (return restart)
        end))

(defun nonstandard-debugger (condition)
  (let* ((*debug-restarts* (compute-filtered-restarts condition))
         (*number-of-debug-restarts* (length *debug-restarts*))
         (*debug-abort* (find-restart 'abort *debug-restarts*))
         (*debug-continue* (or (let ((c (find-named-restart 'continue *debug-restarts*)))
                                 (if (or (not *debug-continue*)
                                         (not (eq *debug-continue* c)))
                                     c nil))
                               (let ((c (if *debug-restarts*
                                            (first *debug-restarts*) nil)))
                                 (if (not (eq c *debug-abort*)) c nil))))
         (*debug-condition* condition))
     (when (cg:simple-stream-p *debugger-window*)
       (cg:shrink-window *debugger-window* t))
     (let ((*debugger-window* (make-debugger-window)))
       (update-debugger-window *debugger-window* condition)
       (cg:reshape-window-exterior *debugger-window*
       (let ((box (cg:nwindow-exterior *debugger-window* '#.(cg:make-box 0 0 0 0))))
         (cg:center-box-on-screen (cg:box-width box) (cg:box-height box))))
       (cg:pop-up-dialog *debugger-window*))))

(defun debugger-invoke-restart-interactively (restart)
  (invoke-restart-interactively restart))

(defun handle-debugger-select (item new-value old-value)
  (let ((restart-message (cg:find-named-object :restart-message (cg:dialog-item-dialog item))))
    (setf (cg:dialog-item-value restart-message)
            (let ((*expanded-restart-label-p* t))
              (get-restart-label new-value))))
  t)

(defun handle-debugger-stop (item new-value old-value)
  (declare (special *top-level-window*))
  (declare (ignore item new-value old-value))
  (if *debug-abort*
      (debugger-invoke-restart-interactively *debug-abort*)
      (pop-up-message-dialog *top-level-window* "Error"
        "There is no way to stop." cg:error-icon "OK"))
  nil)

(defun handle-debugger-continue (item new-value old-value)
  (declare (special *top-level-window*))
  (declare (ignore item new-value old-value))
  (if *debug-continue*
      (debugger-invoke-restart-interactively *debug-continue*)
      (pop-up-message-dialog *top-level-window* "Error"
        "There is no way to continue." cg:error-icon "OK"))
  nil)

(defun handle-debugger-restart (item new-value old-value)
  (declare (ignore new-value old-value))
  (let* ((dialog (cg:dialog-item-dialog item))
         (restart (cg:dialog-item-value (cg:find-named-object :restarts dialog))))
    (when restart
      (debugger-invoke-restart-interactively restart)))
  nil)

;;; Lisp errors will also show up this way; we need to bind up
;;; a handler so that when we get a Lisp error it too can be 
;;; handled using the debugger without breaking the 
;;; top level loop. It is unfortunate that we need the Lisp
;;; format function here. If the error is continuable we 
;;; need to set up a restart so that we can continue. 
;;;
;;; In the runtime environment we should bind the special
;;; variable *error-hook* to error-handler. Then we will get
;;; Lisp errors showing up through our debugger rather than
;;; through the default ACL debugger. 

(defun error-handler (continue-format error-format arguments)
  (if continue-format
      (apply #'cerror continue-format error-format arguments)
      (apply #'error error-format arguments)))

