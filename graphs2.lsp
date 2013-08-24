;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; More graphs stuff. Mostly this space gives us room to add a new selection system
;;; which allows parts to be manipulated by dragging just as well as the whole 
;;; element. This is needed to manage grids properly. 
;;;
;;; We shouldn't call selected-element until we're sure that this isn't a double
;;; click. If it is, we should handle it as such. We can do this by setting up a
;;; pending selection, and cancelling it if we get a second click at the same
;;; place. These changes have a few further implications that we need to be aware of. 
;;; By getting the selected part after the selection, things may break. We only really
;;; want to do this when the element was already selected. 
;;;
;;; The most critical function is the one which which looks through the graph
;;; for the front-most element which matches the mouse position. This should probably
;;; also return some kind of a part code which will act as a hint to the tracking
;;; system to do some command or other. 
;;;
;;; Problem: we're not clicking through link elements properly, because we find the
;;; last value and assume it's OK.  This is a false assumption.  If the last element
;;; is a link, it might still reject the click, and push us to another element or 
;;; even the parent.  In other words, we need to try all elements which might make
;;; sense before giving up.  This looks less efficient, but probably isn't that bad.  
;;;
;;; Recursion is more or less inevitable here.  The idea is to simply use the part
;;; itself as a better cue.  get-element is partitioned neatly inside get-element-at-position
;;; so we can make it work rather differently inside if we want. 

(defun get-element (parent position test &aux value)
  (let ((part (graph-element-part parent position)))
    (if (eq part t)
        (if (cg:inside-box-p position (graph-element-interior parent))
            (loop for element in (graph-elements parent)
                  if (and (cg:inside-box-p position (graph-element-extent element))
                          (or (null test) (funcall test element)))
                    do (setf value element)
                  end
                  finally do (if value
                                 (return (get-element value position test))
                                 (return (values parent t))))
            (values parent t))
        (when part
          (values parent part)))))

(defun get-element-at-position (window position &key test)
  (multiple-value-bind (element part) (get-element window position test)
    (if (eq element window)
        nil
        (values element part))))

(defun call-graph-select-loop (window event buttons position time)
  (multiple-value-bind (element part) (get-element-at-position window position)
    (graph-select-loop window element part event buttons position time)))
     
(defmethod graph-select-loop ((window graph-window) element part event buttons position time)
  (let* ((*draggedp* ())
         (context (make-drag-element-context window)))
    (declare (special *draggedp*))
    (call-select-event window event buttons position context)
    (select-loop window context)
    (setf (getf (cg:stream-plist window) 'pending-selection) nil)
    (unless *draggedp*
      (if element
          (setf (getf (cg:stream-plist window) 'pending-selection) (list time element (copy-part part)))
          (selected-element nil nil window))
      (setf part (when element (graph-element-part element position)))
      (graph-element-mouse-moved window element part))))

(defmethod selected-element (element part window)
  ())

(defmethod canvas-event ((window graph-window) (event (eql cg:mouse-left-down)) buttons position time)
  (call-graph-select-loop window event buttons position (+ time (cg:double-click-time window))))

(defmethod canvas-event ((window graph-window) (event (eql cg:mouse-right-down)) buttons position time)
  (call-graph-select-loop window event buttons position (+ time (cg:double-click-time window))))

(defmethod canvas-event ((window graph-window) (event (eql cg:timer-event)) buttons position time)
  (let ((pending-selection (getf (cg:stream-plist window) 'pending-selection)))
    (when (and pending-selection
               (>= time (first pending-selection)))
      (setf (getf (cg:stream-plist window) 'pending-selection) nil)
      (selected-element (second pending-selection) (third pending-selection) window))))

;;; As and when the mouse moves, we can handle it by changing the cursor. We won't call this
;;; very often, but when we do it can be fairly effective. First, we need to ask the graph
;;; element which part we're in, and then we can ask the graph element what it wants to do to
;;; reflect that part.

(defmethod canvas-event ((window graph-window) (event (eql cg:mouse-moved)) buttons position time)
  (let* ((element (get-element-at-position window position))
         (part (graph-element-part element position)))
    (graph-element-mouse-moved window element part)))

(defmethod graph-element-mouse-moved ((window graph-window) (element link-element) part)
  (let ((parent (graph-element-parent element)))
    (cond ((graph-element-selected-p element)
           (cg:window-message (application-window-main-window (graph-editor-window window)) 
            "Use the ~~Label commands from the ~~Cards menu to change this link's label")
           (cg:set-cursor cg:*screen* cg:arrow-cursor))
          ((graph-element-selected-p parent)
           (cg:window-message (application-window-main-window (graph-editor-window window)) "Click to select this link")
           (cg:set-cursor cg:*screen* cg:arrow-cursor))
          (t
           (graph-element-mouse-moved window parent t)))))

(defmethod graph-element-mouse-moved ((window graph-window) element part)
  (cg:window-message (application-window-main-window (graph-editor-window window)) "")
  (cg:set-cursor cg:*screen* cg:arrow-cursor))

(defmethod graph-element-mouse-moved ((window graph-window) (element null) part)
  (cg:window-message (application-window-main-window (graph-editor-window window)) "Use the ~~Cards menu to make a new Fact or Instruction card")
  (cg:set-cursor cg:*screen* cg:arrow-cursor))

;;; If we get a character event while we're checking out a pending selection, we should
;;; immediately set up the selection and get it to handle the new data. This is something
;;; that is a bit nasty, but probably no more nasty than the Finder is when it is doing
;;; the same kind of thing. 

(defmethod canvas-event ((window graph-window) (event (eql cg:mouse-left-double-click)) buttons data time)
  (declare (ignore event buttons data time))
  (setf (getf (cg:stream-plist window) 'pending-selection) nil)
  (:open-selection window))

;;; Interaction handling code for graphs. This involves mouse handling,
;;; selection handling, the interface to the drag-and-drop system, and
;;; generally almost everything that connects user events to the 
;;; graph system and back again. A key part of this is that we need to be able
;;; to detect where the mouse went down in a graph and handle it accordingly.
;;; This probably means some kind of integration with the drag-and-drop 
;;; system, which is a bit less than ideal. 
;;;
;;; Dragging with the left mouse button should take all the selected 
;;; elements and turn them into a box drag. We will have several items
;;; in this drag, one for each selected item in the graph. 

(defclass drag-move (drag-boxes)
  ())

;;; The dragger for the right button should be rather different. Here
;;; we want to extend drawing by tracking a line rather than a box. 
;;; That is why we don't use 'drag-boxes as the drag class. 
;;;
;;; The drag attachment class is a specialised drag class which marks 
;;; attachment points. We have an attachment point slot, and if
;;; the point changes then we move it, otherwise we leave it alone.
;;; The position can also be set to be nil, in which case we hide it.
;;; It is initially nil. The drag attachment line should also snap to the 
;;; attachment point. 

(defclass drag-attachment (drag-line)
  ((current-attachment
     :initform nil
     :initarg :current-attachment
     :reader drag-attachment-current-attachment)
   (source-attachment
     :initform nil
     :initarg :source-attachment
     :reader drag-attachment-source-attachment)))

(defstruct (drag-element-context
             (:type list)
             (:constructor make-drag-element-context (graph)))
  (element () :read-only t))

;;; The beginning of the interaction code for graphs. This needs to set up
;;; the dragging context, so far as there is one, and the call the event
;;; loop recursively after rebinding the event handler. 
;;;
;;; The first thing that happens every time the mouse goes down is that we
;;; go into the select loop. After this, if we encounter a start-drag event
;;; we go into the drag handler. Return from the drag handler should 
;;; quit the selection immediately. 
;;;
;;; When we get the start drag event with the right button pressed, we should 
;;; then invoke a drag, and finally quit our current select loop.
;;;
;;; Now for the select loop stuff. A function which sets up the
;;; right context for the select loop and then goes into the loop.
;;; You can quit the loop by throwing to 'quit-select.
;;;
;;; During the selection stuff we should really use the context enough to be
;;; able to decide whether to use single or multiple selection codes. 
;;;
;;; Besides this, the interaction style should be a bit smarter. For single
;;; selection, for instance, we should always deselect things when we move 
;;; away from them. 

(defmethod graph-select-event-mouse-handler ((window graph-window) event position buttons)
  (multiple-value-bind (element part) (get-element-at-position window position)
    (let* ((editor-window (graph-editor-window window))
           (main-window (application-window-main-window editor-window)))
      (cond ((cg:button-match buttons cg:shift-key)
             (cond ((null element))
                   ((graph-element-selected-p element)
                    (record-command main-window
                      (make-compound "Also Unselect" (generate-objects-resolver (list element) window)))
                    (setf (graph-element-selected-p element) nil))
                   (t
                    (record-command main-window
                      (make-compound "Also Select" (generate-objects-resolver (list element) window)))
                    (setf (graph-element-selected-p element) t))))
            ((null element)
             (record-command main-window "Unselect All")
             (loop for graph-element in (graph-selected-elements window)
                   do (unless (eq graph-element element)
                        (setf (graph-element-selected-p graph-element) nil))))
            (t
             (record-command main-window
               (make-compound "Select" (generate-objects-resolver (list element) window)))
             (loop for graph-element in (graph-selected-elements window)
                   do (unless (eq graph-element element)
                        (setf (graph-element-selected-p graph-element) nil)))
             (setf (graph-element-selected-p element) t))))))

(comtab:defmenu-command :select ((window graph-window))
  (let ((command *script-command*))
    (when command
      (set-selection window (resolve-objects command (graph-editor-window window))
        :deselectp t))))

(comtab:defmenu-command :also-select ((window graph-window))
  (let ((command *script-command*))
    (when command
      (set-selection window (resolve-objects command (graph-editor-window window))
        :deselectp nil))))

;;; A second strategy is to use the graph drag handler immediately we get
;;; called with a selection event if the mouse is still down. This will 
;;; handle dragging. In fact, I still prefer this strategy. Somehow, we
;;; need to know when the mouse goes up, whether or not there has been
;;; a drag. We should get this through the select handler rather than the
;;; drag handler. 

(defmethod select-event ((window graph-window) (event (eql cg:mouse-left-down)) position modifiers context)
  (graph-select-event-mouse-handler window event position modifiers))

(defmethod select-event ((window graph-window) (event (eql cg:mouse-right-down)) position modifiers context)
  (graph-select-event-mouse-handler window event position modifiers))

;;; Now for the big stuff. We can now start to implement the moving
;;; operation using the drag-and-drop system. This is sensible because
;;; we can even drop the object somewhere. 
;;;
;;; The revisions to the drag and drop system mean that we need to 
;;; think all this through again. It should be a bit simpler this
;;; time. 

(defmethod select-event ((window graph-window) (event (eql :start-drag)) position buttons context)
  (declare (special *draggedp*))
  (complete-element-editor window :end)
  (cg:set-cursor window cg:arrow-cursor)
  (setf *draggedp* t)
  (call-graph-drag-handler window buttons position context))

(defun call-graph-drag-handler (window buttons position context)
  (let* ((element (get-element-at-position window position))
         (part (when element
                 (graph-element-part element position))))
    (graph-drag-handler window element part buttons position)
    (throw 'quit-select nil)))

;;; For a given item we should be able to get a box which we can use
;;; as an area value. This we can do by looking up the item in the
;;; items table, and getting the corresponding box and converting
;;; it to local coordinates.

(defun get-drag-move-item-box (drag item)
  (let* ((items (drag-items drag))
         (position (position item items :test #'equal)))
    (when position
      (let ((box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (elt (drag-boxes-boxes drag) position)))
            (window (drag-current-window drag)))
        (screen-to-window-units window box)
        (list (cg:box-left box) (cg:box-top box) (cg:box-right box) (cg:box-bottom box))))))

;;; Dragging elements no longer needs to be integrated with the document 
;;; system quite so fully, because we've abandoned the document system in
;;; HANK. For now, we can stick with a simple system which will handle 
;;; things rather better. 

(defmethod graph-drag-elements ((window graph-window) buttons position)
  (let* ((selected-graph-elements (graph-selected-elements window))
         (boxes (map 'list #'(lambda (graph-element)
                               (let ((box (cg:copy-box (graph-element-area graph-element))))
                                 (window-to-screen-units window box)))
                    selected-graph-elements))
         (drag (make-drag 'drag-move window buttons 
                 (window-to-screen-units window (cg:copy-position position))
                 :boxes boxes
                 :items selected-graph-elements)))
      (drag-track drag)))

;;; Actually, although I've rather neglected it so far, it probably is a good idea to
;;; use Ousel's command system here. Without this, all the undo stuff will be rather 
;;; hard to implement. Secondly, a lot of dragging and dropping stuff would also be
;;; better if we could use either Procyon's code, or if we could use something which
;;; linked to the Apple system underneath. The Apple system would clearly be better, but
;;; will probably be harder to do, particularly since there's no support for it in the
;;; basic Procyon or even in the toolbox interface it provides. 

(defun get-drag-move-item-box (drag item)
  (let* ((items (drag-items drag))
         (position (position item items :test #'equal)))
    (when position
      (let ((box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (elt (drag-old-tracking drag) position)))
            (window (drag-current-window drag)))
        (screen-to-window-units window box)
        (list (cg:box-left box) (cg:box-top box) (cg:box-right box) (cg:box-bottom box))))))

;;; With this, we can build and execute a command to move items in the graph when the
;;; command is completed. We should make this command undoable, at least eventually.
;;; But perhaps the most important thing is to check that the graphics are handled 
;;; properly. 

(defun drag-command-move-do-function (command)
  (move-elements (command-do-data command)))

(defun drag-command-move-undo-function (command)
  (move-elements (command-undo-data command)))

(defun prepare-to-update-link-area (element)
  (let* ((connection (link-element-connection element))
         (from-point (contact-point (connection-from connection)))
         (from-element (contact-element (connection-from connection)))
         (to-point (contact-point (connection-to connection)))
         (to-element (contact-element (connection-to connection)))
         (segment (create-segment)))
    (nmake-segment segment
      (nattachment-point-position (graph-element-area from-element) 
        '#.(cg:make-position 0 0) from-point) from-point
      (nattachment-point-position (graph-element-area to-element) 
        '#.(cg:make-position 0 0) to-point) to-point)
    (setf (link-element-segment element) (nthcdr (first segment) (rest (rest segment))))))

(defun move-elements (moves)
  (let ((links ()))
    (loop for move in moves
          for element = (first move)
          if (typep element 'node-element)
            do (setf links (nunion links (node-element-links element))))
    (loop for move in moves
          for item = (first move)
          for box = (second move)
          do (setf (graph-element-area item) box))
    (loop for element in links
          do (prepare-to-update-link-area element)
             (setf (graph-element-area element)
                     (cg:copy-box (calculate-link-element-area element))))))

;;; When we move an element, we also move one end of the connected links.
;;; This needs to be managed through updating the new segment. 
;;;
;;; The "Move" command needs to be scriptable. This is not completely trivial,
;;; because it means we need references to things. Finally, although moving
;;; can, in principle, move things to different places, in practice, all the
;;; movements are relative. Finally, we need to make a decision some time about
;;; how we handle screen resolutions in scripts and in other textual forms. 
;;; The point representation used so far is a bit nastily grainy. A better 
;;; strategy would be to normalise the file representation to use something
;;; really fine grained, such as thousands of an inch. Points will easily 
;;; convert across, but we shouldn't lose a lot in rounding. 

(defmethod drag-drop-window ((drag drag-move) (window graph) buttons position)
  (apply-move window (graph-editor-window window)
    (loop for item in (drag-items drag)
          for box = (get-drag-move-item-box drag item)
          if box
            collect (list item (apply #'cg:make-box box)))))

(defun apply-move (window modify-window moves)
  (let ((elements (loop for (element area) in moves
                        for old-area = (graph-element-area element)
                        if (not (cg:box= area old-area))
                          collect (list element area (cg:copy-box old-area)))))
    (when elements
      (record-command (application-window-main-window modify-window)
        (make-compound "Move"
          (loop for (element area old-area) in elements
                collect (generate-objects-resolver (list element) window)
                collect (scale-to-internal
                          (list (cg:box-left area) (cg:box-top area)
                                (cg:box-right area) (cg:box-bottom area))))))
      (execute (make-command
                 :type 'move-elements
                 :window window
                 :label "Move"
                 :do-data (loop for (element area old-area) in elements
                                collect (list element area))
                 :undo-data (loop for (element area old-area) in elements
                                  collect (list element old-area))
                 :modify-window modify-window
                 :do-function #'(lambda (command) (move-elements (command-do-data command)))
                 :undo-function #'(lambda (command) (move-elements (command-undo-data command))))))))
   

(comtab:defmenu-command :move ((window graph-window))
  (let ((command *script-command*))
    (when command
      (apply-move window (graph-editor-window window)
        (loop for values = (compound-arguments command) then (rest (rest values))
              for element = (resolve-path (first values) window)
              for area = (apply #'cg:make-box (scale-to-screen (second values)))
              while values
              collect (list element area))))))

(defmethod cg:event ((window graph) (event (eql cg:character-message)) buttons data time)
  ())

(defmethod cg:event ((window graph) (event (eql cg:virtual-key-down)) buttons data time)
  (cond ((pro:i= data machine:vk-left)
         (graph-move-selection-by window -1 0))
        ((pro:i= data machine:vk-right)
         (graph-move-selection-by window 1 0))
        ((pro:i= data machine:vk-up)
         (graph-move-selection-by window 0 -1))
        ((pro:i= data machine:vk-down)
         (graph-move-selection-by window 0 1))
        ((pro:i= data machine:vk-backspace)
         (:delete window))
        ((or (pro:i= data machine:vk-enter) (pro:i= data machine:vk-return))
         (:open-selection window))
        ((pro:i= data machine:vk-space)
         (when (pro:ilogtest buttons cg:alt-key)
           (cg:event (cg:selected-window cg:*screen*) event buttons data time)))))

(defun graph-move-selection-by (window dx dy)
  (let ((selection (graph-selected-elements window))
        (offset (cg:nmake-position '#.(cg:make-position 0 0) dx dy)))
    (apply-move window (graph-editor-window window)
      (loop for item in selection
            for box = (graph-element-area item)
            collect (list item (cg:nbox-move (cg:copy-box box) offset))))))

(defmethod graph-drag-handler ((window graph-window) element part buttons position)
  (cond ((eq part :link)
         (graph-drag-link-element window element buttons position))
        ((eq part :resize)
         (graph-drag-resize-element window element buttons position))
        ((integerp part)
         (graph-drag-resize-column window element buttons position part))
        ((eq part :resize-header)
         (graph-drag-resize-header window element buttons position))
        ((and (cg:button-match buttons cg:right-mouse-button) (integerp part))
         (graph-drag-connection window part buttons position))
        ((cg:button-match buttons cg:left-mouse-button)
         (graph-drag-elements window buttons position))))

;;; Create a new element. Quite what class of new element is still a bit vague, because
;;; quite a bit depends on the context. This will do for now. The position of the new
;;; element should be calculated a bit more carefully than this. We should also remember
;;; the old selection, so that the command can recreate it. Basically, all the object
;;; creation should fit into a command so that it can be undone. In fact, we handle this
;;; rather like the system we use to manage text selections. 
;;;
;;; This handling of the clipboard needs to be modified in various ways. We should now
;;; remember that we're handling a hierarchical element system rather than a flat one.
;;; Unless we do this, we can't delete all the things that we ought to be able to delete
;;; and things get screwey. Basically, we need to be able to cut elements which aren't
;;; at the root of the system. We also ought to be able to cut a whole bunch of elements
;;; at different levels in one go! We can probably also remove elements from the cut
;;; which didn't ought to be there, because they're being cut elsewhere, but there's
;;; actually no need to do that. 

(defun object-clipboard-command-do-function (command)
  (object-clipboard-command-apply-function command (command-do-data command)))
(defun object-clipboard-command-undo-function (command)
  (object-clipboard-command-apply-function command (command-undo-data command)))

;;; Data format revised to be:
;;; (selection (parent add delete)*)
;;;
;;; This needs some additional code to handle links. Basically, when elements are
;;; added and removed, references to those links also need to be added and removed. 
;;; This will affect both links and tables, as they are deleted. Note that links are
;;; never changed, only tables. 

(defun object-clipboard-command-apply-function (command data)
  (let* ((window (command-window command))
         (old-selection (graph-selected-elements window))
         (new-selection (first data)))
    (when (cg:closed-stream-p window)
      (return-from object-clipboard-command-apply-function nil))
    (loop for set in (rest data)
          for parent = (first set)
          for objects-to-add = (second set)
          for objects-to-delete = (third set)
          do (loop for element in objects-to-delete
                   for extent = (graph-element-extent element)
                   do (when (typep element 'link-element)
                        (loop for contact in (link-element-connection element)
                              for node = (contact-element contact)
                              do (setf (node-element-links node)
                                         (delete element (node-element-links node)))))
                      (cg:erase-contents-box window extent)
                      (cg:invalidate-window window extent))
             (loop for element in objects-to-add
                   do (when (typep element 'link-element)
                        (loop for contact in (link-element-connection element)
                              for node = (contact-element contact)
                              do (setf (node-element-links node)
                                         (cons element (node-element-links node))))))
             (loop for element in (setf (graph-elements parent)
                                          (nconc (set-difference (graph-elements parent) objects-to-delete)
                                                 objects-to-add))
                   for new-selected-p = (and (member element new-selection) t)
                   for old-selected-p = (and (member element old-selection) t)
                   do (cond ((member element objects-to-delete))
                            ((not (eql new-selected-p old-selected-p))
                             (setf (graph-element-selected-p element) new-selected-p)
                             (unless new-selected-p (setf old-selection (delete element old-selection))))
                            ((member element objects-to-add)
                             (cg:invalidate-window window (graph-element-extent element))))))
    (loop for element in (set-difference old-selection new-selection)
          do (setf (graph-element-selected-p element) nil))))


;;; When a grid element part is selected, we should open a small text editor for
;;; it. We also need to be ready to close the text editor as soon as something 
;;; interesting happens to the window. But that can wait. And as well as storing 
;;; a window, we should store something that will act on the result of the window
;;; if it is used to edit something. This something will usually generate a command
;;; and pass it to the window. 
;;;
;;; Try to put all the non-portable stuff into install-element-editor. We'll need to
;;; fix this so that it works on a PC. Basically, the design should be OK but there
;;; might need to be a bit more hacking involved. We can do this by borrowing the
;;; whole of the small text editor from Ousel, if need be. 

(defclass element-pane (#+Procyon-Common-Lisp mac::editable-text-control-pane
                        #+ACLPC cg:text-edit-pane
                        )
  ()
  (:default-initargs :window-border :none :user-scrollable nil))

;;; We're being a bit naive here, in how we choose the right font. We should really
;;; have a system which gives us the right font for the part, and uses this both for
;;; drawing and getting the editor together. Row height here should actually depend 
;;; on the font. This is a bit tricky. 

(defun install-element-editor (element window box options data properties)
  #+Procyon-Common-Lisp (cg:nbox-move box (cg:position* (cg:nscroll-position window '#.(cg:make-position 0 0)) -1 -1))
  #+ACLPC (progn 
            (pro:iincf (cg:box-left box))
            (pro:iincf (cg:box-bottom box) 2)) ;; (pro:idecf (cg:box-bottom box) 4))
  (when (member :hank-win95-look *features*)
    (pro:idecf (cg:box-left box) 1))
  (let ((control (apply #'cg:open-stream 'element-pane window :io
                   :window-interior box
                   :font (cg:font window)
                   :window-state :shrunk
                   options))
        (data (write-term-to-string data)))
    (ew-write-string (machine:window-handle control) data 0 (length data))
    (ew-set-highlight-range (machine:window-handle control) 0 (length data))
    (cg:select-window control)
    (cg:set-cursor window cg:line-cursor)
    (setf (getf (cg:stream-plist window) 'element-editor) (cons control properties))
    control))

