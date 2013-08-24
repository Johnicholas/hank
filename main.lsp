;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

;;; I have never been happy about the palette, and have now decided to abandon it
;;; in favour of a rather more complex approach to using parts. This will take a few 
;;; goes to get right, but it will probably be worthwhile. We will only remove the 
;;; palette later. 

(defclass main-window (#+Procyon-Common-Lisp cg:application-window
                       #+ACLPC cg:frame-window
                       )
  ((body
     :reader main-window-body))
  (:default-initargs
     :user-scrollable nil
     :window-state :icon))

(defclass application-window (cg:frame-window)
  ((main-window
     :initarg :main-window
     :reader application-window-main-window)))

(defmacro all-windows (main-window)
  `(cg:get-stream-prop ,main-window 'not-special-windows))
(defun window-index (window main-window)
  (1+ (position window (all-windows main-window))))
(defun add-window (window main-window)
  (cg:set-stream-prop main-window 'not-special-windows
    (stable-sort (nconc (all-windows main-window) (list window))
      #'pro:whole-string-lessp
      :key #'cg:stream-title)))
(defun remove-window (window main-window)
  (cg:set-stream-prop main-window 'not-special-windows
    (delete window (all-windows main-window))))

(defmethod container-objects ((window main-window))
  (all-windows window))
(defmethod object-type ((window main-window))
  "Application")

;;; A final top level function, which should be run when we
;;; want to get the HANK up and running. It will do various
;;; kinds of initialisation. This is one of the areas where we'll need to
;;; do a fair bit of work to deal with HANK's interface. A lot of this is
;;; because this is where the PC and the Mac tend to be quite different 
;;; in their interface style. 

(defun open-main-window ()
  (cg:open-stream 'main-window cg:*screen* :io 
    :title *main-window-name*
    :window-state :shrunk))

(defclass body-pane (cg:non-refreshing-pane)
  ())

(defmethod cg:event ((window body-pane) (event (eql cg:mouse-moved)) buttons data time)
  (cg:window-message (cg:window-parent window) ""))

(defmethod cg:user-close ((window body-pane))
  (every #'cg:user-close (cg:windows window)))    

(defmethod cg:device-open ((window main-window) options)
  (prog1 (call-next-method)
    (cg:set-background-color window #.(cg:make-rgb :red 192 :green 192 :blue 192))
    (let ((menu-bar (cg:open-menu () 'cg:menu-bar window :pop-up-p nil)))
      (open-window-menus window menu-bar)
      (let ((interior (cg:nvisible-box window '#.(cg:make-box 0 0 0 0))))
        (nbox-inset interior -1 -1)
        (setf (slot-value window 'body)
                (cg:open-stream 'body-pane window :io
                  :window-border :plain
                  :window-state :shrunk
                  :right-attachment :right
                  :bottom-attachment :bottom
                  :user-scrollable nil
                  :background-color (pc:system-color win:color_appworkspace)
                  :window-exterior interior))
        (cg:select-window (slot-value window 'body))))))

;;; Closing a project entails closing all its documents, offering people
;;; a chance to save them in the process. Once this has been completed
;;; we should close the project document itself. 
;;;
;;; We'll need to integrate this with the event loop rather better at the 
;;; end. We can do this by quitting the system when we are in the runtime
;;; system. 

(defmethod :exit ((window main-window))
  (cg:user-close window))

;;; When we get a :close command we must *never* handle it locally. We
;;; should always find the frontmost window that matches our particular
;;; criterion. 

(defun find-window-if (parent test)
  (loop for window = (cg:front-window parent) then (cg:next-window window) do
    (when (or (null window) (funcall test window))
      (return-from find-window-if window))))

;;; Now we can define some smarter handlers for the :close and the :close-project
;;; commands, using this to make sure that we get hold of the right window.

(defmethod :close ((window main-window))
  (let ((window (find-window-if (main-window-body window) #'identity)))
    (when window
      (:close window))))

(defmethod :close ((window body-pane))
  (let ((window (find-window-if window #'identity)))
    (when window
      (:close window))))

(defmethod :close ((window application-window))
  (cg:user-close window))

;;; I'm adding an extra menu here. I know that it isn't in the manual, but that doesn't
;;; matter that much as yet, as there is almost nothing in it. It is, basically, a
;;; a help menu and will give people a quick look at the credits.

(defun help-dialog (window)
  (pop-up-message-dialog window ""
    (format () "Hank for ~A~%~%Copyright (c) 1997, The Open University.~%~
                Authors: Stuart Watt, Paul Mulholland~%~
                Version ~A"
      (software-type)
      (hank-implementation-version))
    cg:information-icon
    "OK"))

(defmethod :about-application ((window main-window))
  (help-dialog window))

(defun hank-implementation-version ()
  "0.0d1")

;;; Define an event handler so that null events sent to the main window
;;; are forwarded directly to the window under the mouse. This is 
;;; a substitute for passing everything down the window chain, but
;;; should be plenty good enough for now.

(defmethod cg:event ((window main-window) (event (eql cg:timer-event)) buttons data time)
  (let ((position (cg:ncursor-position cg:*screen* '#.(cg:make-position 0 0))))
    (multiple-value-bind (event-window event-position) (get-window-and-position position)
      (when (and (typep event-window 'cg:window) (not (eq window event-window)))
        (cg:event event-window event buttons event-position time))))
  (let ((delayed-commands (getf (cg:stream-plist window) 'delayed-commands)))
    (loop for commands = delayed-commands then (rest (rest commands))
          do (if (null commands)
                 (return nil)
                 (apply (first commands) window (second commands))))
    (setf (getf (cg:stream-plist (cg:stream-location window)) 'delayed-commands) nil)))

(defmethod cg:user-close ((window main-window))
  (let ((menu (cg:window-menu window)))
    (when (and (eql 1 (pop-up-message-dialog window "" 
                        "Are you sure you want to leave Hank?"
                        cg:warning-icon "Yes" "No"))
               (cg:user-close (main-window-body window))
               (call-next-method))
      (labels ((close-menu (menu)
                 (loop for item in (cg:menu-items menu)
                       for value = (cg:menu-item-value item)
                       if (typep value 'cg:menu)
                         do (close-menu value))
                 (close menu)))
        (close-menu menu))
      #+Runtime-System (pro:quit :stop)
      t)))

(defmethod cg:event ((window main-window) (event (eql cg:mouse-moved)) buttons data time)
  (cg:window-message window ""))

