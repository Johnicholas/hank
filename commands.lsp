;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Command handling is completely revised, so that we can take advantage of the concepts 
;;; in MacApp which provide for fairly automatic handling of Undo. 
;;;
;;; The basic command class, and its protocol, is described and implemented here. Commands 
;;; are now first-order objects, rather than functions. When a command is given, it should 
;;; create one of these objects and pass it to the window for handling. It should not 
;;; attempt to call functions directly.

(in-package "HANK")

;;; Although the basic structure of a command is pretty simple, we need something like the
;;; standard command system so that we can find our way to the window that we should apply
;;; the commands to. 

(defconstant *window-commands-limit* 8)

(defun window-commands (window)
  (getf (cg:stream-plist window) 'commands))
(defun (setf window-commands) (value window)
  (setf (getf (cg:stream-plist window) 'commands) value))
(defun add-window-command (window command)
  (let ((commands (window-commands window)))
    (setf (window-commands window)
            (cons command (if (< (length commands) *window-commands-limit*)
                              commands
                              (subseq commands 0 (1- *window-commands-limit*)))))))

(defstruct command
  type
  window
  label
  (undoable-p t)
  (done-p nil)
  do-function
  do-data
  undo-function
  undo-data
  modify-window
  undo-modified-p)

(defun command-doit (command &aux modify)
  (funcall (command-do-function command) command)
  (when (setf modify (command-modify-window command))
    (setf (command-undo-modified-p command) (:modified-p modify))
    (setf (:modified-p modify) t))
  (setf (command-done-p command) t))

(defun command-undoit (command &aux modify)
  (when (command-undoable-p command)
    (funcall (command-undo-function command) command)
    (when (setf modify (command-modify-window command))
      (setf (:modified-p modify) (command-undo-modified-p command)))
    (setf (command-done-p command) nil)))

(defun command-redoit (command &aux modify)
  (when (command-undoable-p command)
    (funcall (command-do-function command) command)
    (when (setf modify (command-modify-window command))
      (setf (:modified-p modify) t))
    (setf (command-done-p command) t)))

;;; A revised undo label handler, which will be called before the menu is shown and allows
;;; the command to manipulate the menu item more fully than had previously been the case.
;;; It can now, for instance, set shortcuts as well as the undo label. 
;;;
;;; The final stage is for us to be able to bind a command to a 
;;; window by executing it. When we do this, we set the command
;;; to be the last command for the window, set up the appropriate
;;; window slot for the command, and then call command-doit.
;;;
;;; Because of the label system and stuff like that, we need a slightly more
;;; complicated system for managing the window commands. We handle a lot of
;;; this by finding the appropriate command window for the command, and then
;;; managing two lists, a list of undo commands and a list of redo commands. 
;;; We do our processing on these two lists. 

(defun execute (command)
  (let ((window (command-window command)))
    (setf (getf (cg:stream-plist window) 'redo-commands) ())
    (let ((commands (cons command (getf (cg:stream-plist window) 'undo-commands))))
      (when (> (length commands) *window-commands-limit*)
        (setf commands (subseq commands 0 *window-commands-limit*)))
      (setf (getf (cg:stream-plist window) 'undo-commands) commands)))
  (command-doit command)
  command)

(defun get-undo-label (main-window)
  (let* ((window (:command-window main-window))
         (command (when window (first (getf (cg:stream-plist window) 'undo-commands)))))
    (cond ((null command)
           (values "Undo" nil))
          ((not (command-undoable-p command))
           (values "Undo" nil))
          (t
           (values (concatenate 'string "Undo " (command-label command)) t)))))

(defun get-redo-label (main-window)
  (let* ((window (:command-window main-window))
         (command (when window (first (getf (cg:stream-plist window) 'redo-commands)))))
    (cond ((null command)
           (values "Redo" nil))
          (t
           (values (concatenate 'string "Redo " (command-label command)) t)))))

;;; We need a lot more command stuff, so that we can deal with scripting. This might seem
;;; a bit advanced, but in practice we'll be using this to generate a lot of the material
;;; for the HANK assignment book. The idea is to make it possible to generate most of the
;;; book material directly. 
;;;
;;; Many commands have to deal with a selection, and this is where things get hard. The
;;; selection needs to be in a form which can be written to a file, and read again. This
;;; is not necessarily easy, because a lot is only represented in a Lispy binary form. 
;;; The main problem is: how do we know which object we're referring to. A lot of this
;;; can be borrowed from AppleScript's concepts of containers. The application, then,
;;; contains a bunch of things that we can use. 

(comtab:defmenu-command :record ((window main-window))
  (let ((menu-item (cg:find-named-object :record (cg:window-menu window)))
        (stream (cg:get-stream-prop window 'script-stream)))
    (assert menu-item)
    (assert (not (acl:xor stream (cg:menu-item-selected-p menu-item))))
    (if stream
        (progn
          (close stream)
          (setf (cg:menu-item-title menu-item) "Start Recording...")
          (setf (cg:menu-item-selected-p menu-item) nil)
          (cg:set-stream-prop window 'script-stream nil))
        (let ((pathname (cg:ask-user-for-new-pathname "Script File..."
                          :allowed-types '(("HANK Script" . "*.hst"))
                          :initial-name "Untitled.hst")))
          (when pathname
            (setf stream (open pathname :if-exists :supersede :if-does-not-exist :create :direction :output))
            (cg:set-stream-prop window 'script-stream stream)
            (cg:set-stream-prop window 'script-time (get-internal-real-time))
            (setf (cg:menu-item-title menu-item) "Stop Recording")
            (setf (cg:menu-item-selected-p menu-item) t))))))

(defun record-wait-and-command (stream main-window command)
  (let ((old-time (cg:get-stream-prop main-window 'script-time))
        (new-time (get-internal-real-time)))
    (format stream
      "~@[Sleep(~4,2,,,'0F).~%~]~A.~%" 
      (when (> new-time old-time)
        (cg:set-stream-prop main-window 'script-time new-time)
        (/ (- new-time old-time) internal-time-units-per-second))
      (write-term-to-string command :escape t))))

(defmacro record-command (main-window form)
  `(let ((stream (cg:get-stream-prop ,main-window 'script-stream)))
     (when stream
       (record-wait-and-command stream ,main-window ,form))))

(defun play-command (main-window command)
  (cg:process-single-event t)
  (let* ((operator (compound-operator command))
         (string (substitute #\- #\Space (string-upcase operator)))
         (symbol (find-symbol string (find-package "KEYWORD")))
         (*script-command* command))
    (declare (special *script-command*))
    (if (fboundp symbol)
        (apply (symbol-function symbol) main-window (get symbol 'play-command-arguments))
        (cg:beep main-window))))

(comtab:defmenu-command :sleep ((window main-window))
  (let ((command *script-command*)
        (menu-item (cg:find-named-object :real-time-p (cg:window-menu window))))
    (when (and command menu-item (cg:menu-item-selected-p menu-item))
      (let ((value (first (compound-arguments command))))
        (when (numberp value)
          (sleep value))))))

(comtab:defmenu-command :real-time-p ((window main-window))
  (let ((menu-item (cg:find-named-object :real-time-p (cg:window-menu window))))
    (assert menu-item)
    (setf (cg:menu-item-selected-p menu-item) (not (cg:menu-item-selected-p menu-item)))))

(comtab:defmenu-command :beep ((window main-window))
  (cg:beep window))

;;; When we play a file, set the stream to nil, to turn off command recording.
;;; We should, though, be good enough to record the play command itself. That
;;; way we can record a play command, but not all the commands in its script. 

(comtab:defmenu-command :play ((window main-window))
  (let ((pathname (or (when *script-command*
                        (let ((file (string-getf (compound-arguments *script-command*) "File")))
                          (and file
                               (probe-file file))))
                      (cg:ask-user-for-existing-pathname "File to Play"
                        :allowed-types '(("HANK Script" . "*.hst"))))))
    (when pathname
      (record-command window
        (make-compound "Play" (list "File" (namestring pathname))))
      (let ((stream (cg:get-stream-prop window 'script-stream)))
        (unwind-protect (progn
                          (cg:set-stream-prop window 'script-stream nil)
                          (loop for term in (get-file-terms pathname)
                                do (cond ((compound-p term)
                                          (play-command window term))
                                         ((stringp term)
                                          (play-command window (make-compound term ()))))))
          (cg:set-stream-prop window 'script-stream stream))))))

(comtab:defmenu-command :inform ((window main-window))
  (let ((command *script-command*))
    (when command
      (let ((value (first (compound-arguments command))))
        (when (stringp value)
          (pop-up-message-dialog window "" value cg:information-icon "OK"))))))

(defgeneric object-named-p (object name))
(defgeneric object-name (object))
(defgeneric container-objects (object))
(defgeneric container-sorted-objects (object))
(defgeneric object-container (object))
(defgeneric object-type (object))

(defmethod object-named-p (object name)
  (string-equal name (object-name object)))

(defmethod container-sorted-objects (object)
  (container-objects object))

(defun set-selection (window elements &key deselectp)
  (when deselectp
    (loop for window-element in (graph-selected-elements window)
          do (unless (member window-element elements)
               (setf (graph-element-selected-p window-element) nil))))
  (loop for element in elements
        do (setf (graph-element-selected-p element) t)))

(defmethod select-object ((stream cg:window))
  (cg:select-window stream))
(defmethod select-object ((object list))
  (loop for element in object
        do (select-object element)))

(comtab:defmenu-command :select-document ((window main-window))
  (let ((command *script-command*))
    (when command
      (let ((objects (resolve-objects command window)))
        (assert (= (length objects) 1))
        (cg:select-window (first objects))))))

(comtab:defmenu-command :select-window ((window main-window))
  (let ((command *script-command*))
    (when command
      (let* ((name (third (compound-arguments command)))
             (windows-menu (cg:find-named-object :window (cg:window-menu window)))
             (item (find-if #'(lambda (item)
                                (pro:whole-string-equal name (cg:menu-item-title item)))
                            (cg:menu-items windows-menu)))
             (*record-select-window-p* nil))
        (funcall (cg:menu-selection-function windows-menu)
          windows-menu item window)))))

;;; Selecting a window is a bit different, because the special windows don't
;;; necessarily exist yet. Create them if required. Again, the hierarchy should
;;; not make a difference. 

(defun resolve-path (path root)
  (cond ((pro:whole-string-equal (first path) "Root")
         (values root (rest path)))
        ((pro:whole-string-equal (second path) "Named")
         (let* ((rest (rest (rest (rest path))))
                (next (first rest))
                (new-object root))
           (when (and rest (not (pro:whole-string-equal (first rest) "And")))
             (multiple-value-setq (new-object rest) (resolve-path (rest rest) root)))
           (values (loop for element in (container-objects new-object)
                         do (when (object-named-p element (third path))
                              (return element)))
                   rest)))
        ((pro:whole-string-equal (second path) "Index")
         (let* ((rest (rest (rest (rest path))))
                (next (first rest))
                (new-object root))
           (when (and rest (not (pro:whole-string-equal (first rest) "And")))
             (multiple-value-setq (new-object rest) (resolve-path (rest rest) root)))
           (values (elt (container-sorted-objects new-object) (1- (third path))) rest)))
        (t
         (error "Can't find object"))))
       
(defun resolve-objects (command root)
  (let ((path (compound-arguments command))
        (objects nil))
    (loop
      (unless path
        (return))
      (multiple-value-bind (object rest) (resolve-path path root)
        (push object objects)
        (setf path rest)
        (when path
          (assert (pro:whole-string-equal (first path) "And"))
          (pop path))))
    (nreverse objects)))

(defun generate-objects-resolver (elements root)
  (let ((paths ()))
    (loop for element in elements
          do (labels ((get-ancestors (element)
                        (if (eq element root)
                            ()
                            (cons element (get-ancestors (object-container element))))))
               (let ((path (get-ancestors element)))
                 (if (null path)
                     (push (list "Root") paths)
                     (push (loop with firstp = t
                                 for element in (nreverse path)
                                 for name = (object-name element)
                                 for parent = (object-container element)
                                 for siblings = (container-sorted-objects parent)
                                 if firstp
                                   do (setf firstp nil)
                                 else
                                   collect "Of" into result
                                 end
                                 if (= 1 (count-if #'(lambda (sibling)
                                                        (object-named-p sibling name)) siblings))
                                   collect (object-name element) into result
                                   and collect "Named" into result
                                 else
                                   collect (1+ (position element siblings)) into result
                                   and collect "Index" into result
                                 end
                                 collect (object-type element) into result
                                 finally do (return (nreverse result)))
                       paths)))))
    (loop with firstp = t
          for element in (nreverse paths)
          if firstp
            do (setf firstp nil)
          else
            collect "And"
          end
          append element)))

(comtab:defmenu-command :resize-table ((window main-window))
  (let ((command *script-command*))
    (when command
      (let ((rows (first (compound-arguments command)))
            (columns (second (compound-arguments command))))
        ()))))

(defgeneric :rename (window name))
(defmethod :rename ((window t) name) nil)
(defmethod :rename ((window pro:simple-stream) name)
  (:rename (cg:selected-window window) name))
(setf (get :rename 'play-command-arguments) (list nil))

(defgeneric :edit (window part value))
(defmethod :edit ((window t) part value) nil)
(defmethod :edit ((window pro:simple-stream) part value)
  (:edit (cg:selected-window window) part value))
(setf (get :edit 'play-command-arguments) (list nil nil))

(defgeneric :resize (window size))
(defmethod :resize ((window t) size) nil)
(defmethod :resize ((window pro:simple-stream) size)
  (:resize (cg:selected-window window) size))
(setf (get :resize 'play-command-arguments) (list nil))

;;; Selection needs to be handled with a bit more care. Basically, we need to be 
;;; able to monitor selection just like anything else. Selection comes in a few different
;;; types. For example, some selections are new, some are changes. We need to handle
;;; all these different cases. And finally, we need to make up object references for
;;; selections as and when they occur.  

;;; Finally, because of new scripting language, there may be commands which we can't
;;; see in the menu system. We can call these commands through an additional 
;;; scripting command, which prompts for the command name and then calls it if 
;;; possible. Commands can be passed arguments, if the term is called in the
;;; right form. Normally, though, nobody would need to be aware of that. 

(comtab:defmenu-command :execute ((window main-window))
  (let ((command *script-command*))
    (if command
        (setf command (first (compound-arguments command)))
        (setf command (multiple-value-bind (string button) (ask-user-for-string "Command to execute"
                                                              "" "OK" "Cancel")
                        (when (pro:i= button 2)
                          (return-from :execute))
                        (handler-case (first (with-input-from-string
                                                 (stream (concatenate 'string string "."))
                                               (get-stream-terms stream)))
                          (error () string)))))
    (when (stringp command)
      (setf command (make-compound command ())))
    (play-command window command)))

(comtab:defmenu-command :inform ((window main-window))
  (let ((command *script-command*)
        (message nil))
    (when command
      (setf message (first (compound-arguments command))))
    (unless message
      (multiple-value-bind (string button) (ask-user-for-string "Message to show" "" "OK" "Cancel")
        (when (pro:i= button 1)
          (setf message string))))
    (when (stringp message)
      (pop-up-message-dialog window "" message cg:information-icon "OK"))
    (record-command window
      (make-compound "Inform" (list message)))))

