;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University
;;;
;;; Even more spreadsheet stuff. This is where we really get down to the nuts and 
;;; bolts of nested tables. I just hope that the graph manager can cope -- and
;;; I am not yet convinced. Even so, we can actually achieve a lot here, particularly
;;; if we switch away from using slot accessors directly for getting at the extent
;;; and area for elements. Note that nested elements should be drawn "within" an
;;; element, and should be clipped to it, so in many ways each element should be a
;;; graph in its own right. This spoils all the selection code, though, so we can't
;;; work completely like that. 

(in-package "HANK")

;;; The origin of the new element depends on the origin of the parent. And
;;; secondly, when we've created the new element, it shouldn't really show 
;;; up much. In particular, when we create a new element inside something
;;; the old element should no longer be selected. There's a lot to do here,
;;; but the hierarchical element system is beginning to happen!! 

(defmethod nset-new-element-position ((window graph-window) position)
  (cg:nmake-position position 10 10)
  position)

(defmethod nset-new-element-position ((element table-element) position)
  (let ((area (slot-value element 'area)))
    (cg:nmake-position position
      (pro:i+ (cg:box-left area) 10)
      (pro:i+ (cg:box-top area) (pro:i* 4 (grid-graph-element-row-height element)))))
  position)

;;; Now we have to handle some other stuff, like the :cut and other, similar, commands.
;;; These depend on the context, to some extent at least. As you'd expect, all these 
;;; commands should be undoable. Note that for these things to work, we need to map the
;;; changes into a textual representation which can actually be copied somewhere. We
;;; can use the file representation for this. 

(comtab:defmenu-command :select-all ((window element-pane))
  (let ((handle (machine:window-handle window)))
    (ew-set-highlight-range handle 0 (ew-length handle))))

(defmethod cg:copy-selection ((window element-pane))
  (multiple-value-bind (start end) (ew-highlight-range (machine:window-handle window))
    (get-window-text window)
    (cg:push-lisp-clipboard cg:*screen* (subseq (teb) start end))))

;;; Cutting the selection is a bit different, because it should actually
;;; change the text in the element. I am not entirely sure that cutting
;;; or copying should be undoable. Copying isn't in ClarisWorks, but 
;;; cutting is. However, once you're outside the element pane's context,
;;; you can't undo it. This is the pattern we should probably follow. 

(defmethod cg:cut-selection ((window element-pane))
  (multiple-value-bind (window selection) (:selection window)
    (cg:push-lisp-clipboard cg:*screen* selection))
  (multiple-value-bind (start end) (ew-highlight-range (machine:window-handle window))
    (make-element-clipboard-command window 'cut-elements "Cut" start end "")))

(defmethod cg:delete-selection ((window element-pane))
  (multiple-value-bind (start end) (ew-highlight-range (machine:window-handle window))
    (make-element-clipboard-command window 'delete-elements #+Windows "Delete" #+Macintosh "Clear" start end "")))

(defmethod cg:paste-selection ((window element-pane) string)
  (multiple-value-bind (start end) (ew-highlight-range (machine:window-handle window))
    (make-element-clipboard-command window 'paste-elements "Paste" start end string)))

(comtab:defmenu-command :command-window ((window element-pane))
  window)
 
(defun make-element-clipboard-command (window type label start end value)
  (let ((command (make-command
                   :type type
                   :window window
                   :modify-window window
                   :label label
                   :do-data (list start end value)
                   :undo-data (let ((handle (machine:window-handle window)))
                                (get-window-text window)
                                (list start (pro:i+ start (length value)) (subseq (teb) start end)))
                   :do-function #'element-clipboard-command-do-function
                   :undo-function #'element-clipboard-command-undo-function)))
    (execute command)))

(defun element-clipboard-command-do-function (command)
  (element-clipboard-command-apply-function command (command-do-data command) t))
(defun element-clipboard-command-undo-function (command)
  (element-clipboard-command-apply-function command (command-undo-data command) nil))

(defun element-clipboard-command-apply-function (command data insertionp)
  (let* ((window (command-window command))
         (handle (machine:window-handle window)))
    (get-window-text window)
    (ew-set-highlight-range handle (first data) (second data))
    (ew-delete handle)
    (let* ((value (third data))
           (length (length value))
           (start  (first data))
           (end (pro:i+ start length)))
      (ew-write-string handle value 0 length)
      (if insertionp
          (ew-set-highlight-range handle end end)
          (ew-set-highlight-range handle start end)))))

;;; As a special hack, we need to handle typing. Typing can do practically anything,
;;; so we want to save the whole field at the beginning, and at the end of every other
;;; command. Then, when we do a command, we can handle typing as a special case, and
;;; flush the typing command. In effect, the typing command is started immediately we
;;; start the element editor, with unto data, and its do data is only set when another
;;; command is executed, or when the undo command is actually done. The typing command,
;;; therefore, accumulates. 
;;;
;;; A patch! Stop processing command keys in editable text windows when they don't
;;; match a menu.

(defmethod cg:event ((window element-pane) (event (eql cg:virtual-key-down)) buttons data time)
  (unless (pro:ilogtest cg:control-key buttons)
    (call-next-method)))

;;; We now want to build commands which can change the type of an element. As part
;;; of this, the :class of the table should be changed, and any relevant area 
;;; needs to be recalculated. Finally, before this happens, we want to ensure that
;;; the element editor has been completed. This is where things start to get big, and
;;; I am beginning to think there will probably be at least four table files before 
;;; the whole system works. 

(defun change-class-command-apply-function (command data)
  (let ((window (command-window command)))
    (loop for pair in data
          for element = (first pair)
          for class = (second pair)
          do (cg:erase-contents-box window (graph-element-extent element))
             (cg:invalidate-window window (graph-element-extent element))
             (setf (slot-value element 'class) class)
             (setf (graph-element-area element t)
                     (cg:copy-box (calculate-table-element-area element)))
             (cg:invalidate-window window (graph-element-extent element)))))

(defun change-class-command-do-function (command)
  (change-class-command-apply-function command (command-do-data command)))
(defun change-class-command-undo-function (command)
  (change-class-command-apply-function command (command-undo-data command)))

(comtab:defmenu-command :fact-card ((window graph))
  (apply-change-class-command window "Fact Card" :fact_card))
(comtab:defmenu-command :instruction-card ((window graph))
  (apply-change-class-command window "Instruction Card" :instruction_card))
(comtab:defmenu-command :question-box ((window graph))
  (apply-change-class-command window "Question Box" :question_box))
#||
(comtab:defmenu-command :special-box ((window graph))
  (apply-change-class-command window "Preprogrammed Box" :special_box))
||#

(defun apply-change-class-command (window label class)
  (let* ((elements (graph-selected-elements window))
         (change-elements (remove class elements :key #'table-element-class))
         (command (make-command
                    :type 'change-element-class
                    :window window
                    :modify-window window
                    :label label
                    :do-data (loop for element in change-elements
                                   collect (list element class))
                    :undo-data (loop for element in change-elements
                                     collect (list element (table-element-class element)))
                    :do-function #'change-class-command-do-function
                    :undo-function #'change-class-command-undo-function)))
    (execute command)))

(defmethod grid-graph-element-cells ((table-element table-element))
  (with-slots (class template row-data body-data body-cells) table-element
    (let ((v-class class)
          (v-body-cells body-cells)
          (v-template template))
      (cg:nmake-position '#.(cg:make-position 0 0)
        (case v-class
          ((:fact_card :question_box :trace_box)
           (pro:imax 1 (cg:position-x v-body-cells)))
          ((:instruction_card #|| :command_card :special_box ||#)
           (cg:position-x v-body-cells))
          ((:question_box)
           (if v-template
               (pro:imax 1 (length v-template))
               (pro:imax 1 (length (first body-data))))))
        (case v-class
          ((:fact_card :trace_box) (pro:i1+ (cg:position-y v-body-cells)))
          ((:instruction_card) 2)
          ((:question_box) (if v-template 2 1))
          #|| ((:command_card :special_box) (if v-template 1 0)) ||#)))))

;;; Now we are getting to the point where we can look at the system with a bit more
;;; care. First off, not all tables have attachment points. In practice, only questions
;;; and special tables do. Second, we can now develop the grid and element drawing a bit
;;; further. Finally, at this point we can begin to assume the printing system and use
;;; it to manage mapping tables into a displayable form. 

(defmethod grid-graph-element-cell-data ((table-element table-element) cell)
  (with-slots (class template row-data body-data) table-element
    (let ((x (cg:position-x cell))
          (y (cg:position-y cell)))
      (or (case class
            ((:fact_card :trace_box) 
             (if (pro:izerop y)
                 (nth x template)
                 (nth x (nth (pro:i1- y) row-data))))
            ((:instruction_card)
             (if (pro:izerop y)
                 (nth x template)
                 (nth x (first body-data))))
            ((:question_box)
             (let ((header template))
               (if (and header (pro:izerop y))
                   (nth x header)
                   (nth x (first row-data)))))
            #|| ((:command_card :special_box)
                 (nth x template)) ||#)
          ""))))

(defgeneric (setf table-element-body-cells) (value table-element &optional recursivep))
(defmethod (setf table-element-body-cells) (value (element table-element) &optional recursivep)
  (when recursivep
    (cg:ncopy-position (slot-value element 'body-cells) value)
    (return-from table-element-body-cells value))
  ())

;;; The cell data, again, is dependent on the class of object that we're interested
;;; in. This means we need to check the element class a bit. 

(defmethod (setf grid-graph-element-cell-data) (value (element table-element) cell &optional recursivep)
  (when recursivep
    (let ((x (cg:position-x cell))
          (y (cg:position-y cell)))
      (cond ((pro:izerop y)
             (let ((value (replace-element value (slot-value element 'template) x)))
               (setf (slot-value element 'template) value)
               x))
            ((eq (slot-value element 'class) :instruction_card)
             (let ((value (replace-element value (slot-value element 'body-data) 0 x)))
               (setf (slot-value element 'body-data) value)
               value))
            (t
             (let ((value (replace-element value (slot-value element 'row-data) (pro:i1- y) x)))
               (setf (slot-value element 'row-data) value)
               value))))
    (return-from grid-graph-element-cell-data value))
  (setf (grid-graph-element-cell-data element cell t) value)
  (let ((box (grid-graph-element-cell-box element cell))
        (window (graph-element-window element)))
    (when window
      (cg:erase-contents-box window box)
      (redraw-clipped-graph window (graph-element-parent element) box))
    value))

(defmethod (setf table-element-title) (value (element table-element) &optional recursivep)
  (when recursivep
    (setf (slot-value element 'title) value)
    (return-from table-element-title))
  (setf (table-element-title element t) value)
  (let ((box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-area element)))
        (window (graph-element-window element)))
    (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) (grid-graph-element-row-height element)))
    (when window
      (cg:erase-contents-box window box)
      (redraw-clipped-graph window (graph-element-parent element) box))))

;;; When the area of a table element is changed, all its connections may also
;;; need to be updated. This will generally involve recalculating all its
;;; segments. We may also need to expand the area of the enclosing element, 
;;; so that things are handled appropriately. 

(defmethod (setf graph-element-area) (value (graph-element table-element) &optional recursivep
                                      &aux (old-value '#.(cg:make-box 0 0 0 0)))
  (call-next-method))

(defmethod object-name ((element table-element))
  (table-element-title element))
(defmethod object-name ((element link-element))
  ())
(defmethod object-type ((element table-element))
  "Card")
(defmethod object-type ((element link-element))
  "Link")

;;; We can now begin to add a few more helpful commands. These do not need to be built
;;; into the menu system, at least for now. Instead, we can define them as standard
;;; commands and then use them from the scripting system. These commands are, on the
;;; whole, commands which aren't meant for normal mortals, but are instead the kind
;;; of thing that a scripting system might find useful.
;;;
;;; Frame takes the selected element and changes its area so that it encloses all its
;;; children by an ideal inset. It is useful when you've created an Hank: Ask table
;;; enclosing a question, as framing the enclosing element will make everything nice
;;; and neat inside. It does need to be handled with a little care, though, as when
;;; doing nested framing there is a lot which can go wrong.  

(comtab:defmenu-command :frame ((window graph-window))
  (let ((elements (graph-selected-elements window)))
    (if (and (pro:i= (length elements) 1)
             (typep (first elements) 'table-element)
             (graph-elements (first elements)))
        (let* ((element (first elements))
               (children (graph-elements element))
               (box (cg:copy-box (graph-element-area (first children))))
               (interior (graph-element-interior element))
               (moves ())
               (resizes ()))
          (pro:iincf (cg:box-right box))
          (pro:iincf (cg:box-bottom box))
          (loop for child in (rest children)
                do (nbox-union box (graph-element-area child)))
          (nbox-inset box (pro:i- *table-inset*) (pro:i- *table-inset*))
          (let* ((dx (pro:i- (cg:box-left box) (cg:box-left interior)))
                 (dy (pro:i- (cg:box-top box) (cg:box-top interior)))
                 (offset (cg:nmake-position '#.(cg:make-position 0 0) (pro:i- dx) (pro:i- dy))))
            (unless (and (pro:izerop dx) (pro:izerop dy))
              (loop for child in children
                    do (push (list child (cg:nbox-move (cg:copy-box (graph-element-area child)) offset)) moves)))
            (cg:nbox-move box offset))
          (let ((sx (pro:i- (cg:box-right box) (cg:box-right interior)))
                (sy (pro:i- (cg:box-bottom box) (cg:box-bottom interior))))
            (unless (and (pro:izerop sx) (pro:izerop sy))
              (let ((area (cg:copy-box interior)))
                (pro:iincf (cg:box-right area) sx)
                (pro:iincf (cg:box-bottom area) sy)
                (push (list element (cg:make-position (cg:box-width area) (cg:box-height area))) resizes))))
          (when (or moves resizes)
            (let* ((do-data `(:resizes ,resizes :moves ,moves))
                   (undo-data `(:resizes ,(loop for (element size) in resizes
                                                collect (list element (cg:copy-position (table-element-body-size element))))
                                :moves ,(loop for (element area) in moves
                                              collect (list element (cg:copy-box (graph-element-area element))))))
                   (command (make-command
                             :type 'frame-elements
                             :window window
                             :label "Frame"
                             :do-data do-data
                             :undo-data undo-data
                             :modify-window (graph-editor-window window)
                             :do-function #'frame-do-function
                             :undo-function #'frame-undo-function)))
              (execute command)
              (record-command (application-window-main-window (graph-editor-window window)) "Frame"))))
        (cg:beep window))))

(defun frame-do-function (command)
  (frame-elements (command-do-data command)))
(defun frame-undo-function (command)
  (frame-elements (command-undo-data command)))

(defun frame-elements (data)
  (move-elements (getf data :moves))
  (loop for (element size) in (getf data :resizes)
        do (resize-container-element element size)))

;;; When we're framing something, we need to resize it as well as move all its
;;; children. When resizing, we may need to change the body size. 