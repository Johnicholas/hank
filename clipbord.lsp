;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

(defgeneric :selection (window &optional no-text-p))
(defmethod :selection ((window t) &optional no-text-p) nil)
(defmethod :selection ((window pro:simple-stream) &optional no-text-p)
  (:selection (cg:selected-window window) no-text-p))

(defmethod :selection ((window graph) &optional no-text-p)
  (unless no-text-p
    (let ((editor-descriptor (getf (cg:stream-plist window) 'element-editor)))
      (when editor-descriptor
        (return-from :selection (:selection (first editor-descriptor))))))
  (values window (graph-selected-elements window)))

;;; We can now begin to handle some of the more complex stuff that's needed. For
;;; example, we need to be able to resize elements, and to connect elements. 
;;; These need graphical stuff to make them work properly. We also want to be 
;;; able to create elements and to be able to edit their sizes, particularly their
;;; columns. This requires us to be a bit more sophisticated about our selection
;;; system. Selection is still pretty naive. We want to be able to double-click on
;;; cells, and yet to single click to select them as small editors. We also want to
;;; be able to do range selection on them, for example, to be able to delete rows. 
;;; For now, though, that can wait. We can stick with simple code for handling this
;;; for a whole grid. It would still be nice if we could have a deferred selection
;;; system, though. This implies that a double click doesn't get sent to the small
;;; text editor. 

(comtab:defmenu-command :select-all ((window graph-window))
  (let ((editor-descriptor (getf (cg:stream-plist window) 'element-editor)))
    (when editor-descriptor
      (return-from :select-all (:select-all (first editor-descriptor)))))
  (labels ((select (parent)
             (loop for element in (graph-elements parent)
                   do (select element))
             (when (and (typep parent 'graph-element) (not (graph-element-selected-p parent)))
               (setf (graph-element-selected-p parent) t))))
    (select window))
  (record-command (application-window-main-window (graph-editor-window window))
    "Select All"))

(comtab:defmenu-command :unselect-all ((window graph-window))
  (loop for element in (graph-selected-elements window)
        do (setf (graph-element-selected-p element) nil))
  (record-command (application-window-main-window (graph-editor-window window))
    "Unselect All"))

;;; When we're looking for something on the clipboard, it will either be Hank text for plain
;;; text, although both are handled identically as far as the operating system is concerned.
;;; To decide what it is, we need to check the first symbol. We should use this to hold any
;;; additional properties needed by the system. It will be a static table, added to 
;;; contain such interesting things as the window size and position. 
;;;
;;; Note that hank-text-p is also used to flag text as containing stuff which can be
;;; used on the clipboard. 

(defun hank-text-p (string)
  (with-input-from-string (stream string)
    (multiple-value-bind (class value) (buffer-read-token (make-tokeniser stream))
      (and (eq class 'name)
           (string-equal value *system-tag-name*)))))

;;; When handling copy selection, we really to add an extra marker type which we can use when we
;;; come to handle the text. Actually, the simplest solution to this is simply to ensure that
;;; we know the returned value isn't some normal text.  Do this by putting the text into a 
;;; string, but also embedded a non-ASCII code in there somewhere, something hard (or unlikely) to be
;;; typed.  This can then be recognised when pasting.  
;;;
;;; Alternatively, and perhaps more usefully, if we used the Prolog like notation that would be the
;;; kind of thing we could parse anyway.  

(defmethod cg:copy-selection ((window graph))
  (let ((editor-descriptor (getf (cg:stream-plist window) 'element-editor)))
    (when editor-descriptor
      (return-from cg:copy-selection (cg:copy-selection (first editor-descriptor)))))
  (let ((elements (loop for (element graph-element) in (get-file-elements (graph-selected-elements window))
                        collect element)))
    (unless elements
      (return-from cg:copy-selection (values window "")))
    (values window
      (pc::cr-to-crlf
        (with-output-to-string (output)
          (write-terms-to-stream 
            (cons (make-compound *system-tag-name* (list "Clipboard")) elements)
            output))))))

(comtab:defmenu-command :duplicate ((window graph))
  (let* ((selected-elements (graph-selected-elements window))
         (elements (get-file-elements (remove-descendants window selected-elements))))
    (unless elements
      (return-from :duplicate nil))
     
    ;; With these file elements, we can then put new elements back in the places we want.  We 
    ;; should, therefore, keep track of the appropriate file element.  
    ;;
    ;; The only difficult bit is to make these elements back into appropriate graph elements in the
    ;; appropriate places.  
     
    (labels ((move (elements)
               (loop for element in elements
                     do (when (compound-p element)
                          (let* ((properties (second (compound-arguments element)))
                                 (area (string-getf properties "Area")))
                            (when area
                              (incf (first area) 100)
                              (incf (second area) 100)
                              (incf (third area) 100)
                              (incf (fourth area) 100))
                            (move (rest (rest (compound-arguments element)))))))
               elements))
      (move (mapcar #'first elements))
      
      (let* ((elements (loop for (element graph-element) in elements
                             collect (let* ((parent (graph-element-parent graph-element))
                                            (elements (get-graph-elements-from-forms (list element) parent)))
                                       (loop for element in elements
                                             do (set-window element window))
                                       (list parent elements))))
             (command (make-command
                        :type 'duplicate-elements
                        :window window
                        :do-data (cons (reduce #'union elements :key #'second)
                                       (loop for (parent new-elements) in elements
                                             collect (list parent new-elements ())))
                        :undo-data (cons selected-elements
                                         (loop for (parent new-elements) in elements
                                               collect (list parent () new-elements)))
                        :label "Duplicate"
                        :do-function #'object-clipboard-command-do-function
                        :undo-function #'object-clipboard-command-undo-function)))
        (execute command)))))

;;; Duplicating elements is rather easier than copying and pasting them, because each 
;;; element goes into the context that it came from. Links and stuff should also be handled
;;; but that's getting a bit harder.  Also, for duplicate, we can add a restriction that 
;;; only one element should be allowed.  

(defmethod cg:paste-selection ((window graph) object)
  (let ((editor-descriptor (getf (cg:stream-plist window) 'element-editor))
        (hank-text-p (and (stringp object) (hank-text-p object))))
    (when (and (stringp object) (not hank-text-p) editor-descriptor)
      (return-from cg:paste-selection (cg:paste-selection (first editor-descriptor) object)))
    (when hank-text-p
      (setf object (with-input-from-string (input object)
                     (get-stream-terms input))))
     
    (pop object)
     
    ;;; At this point, we know we've got some objects.  Now we have to work out what to do 
    ;;; with them, given that they may well not be compatible with the current context. This is
    ;;; probably why I gave up cut and paste at least twice before.  
     
    (print object)))

;;; Revise the code which deletes the selection to use all the power of the
;;; new clipboard command system. This can definitely be a bit smarter than
;;; this version. However, we do need to be a bit more cunning about the use
;;; of the clipboard. For example, we should really interface it to the file
;;; system and use the strings it generates (at least for now) as the clipboard
;;; format. Then we can genuinely copy and paste. However, copying stuff with
;;; the hierarchical freedom of deleting will never be completely possible. 

(defmethod cg:delete-selection ((window graph))
  (let ((editor-descriptor (getf (cg:stream-plist window) 'element-editor)))
    (when editor-descriptor
      (return-from cg:delete-selection (cg:delete-selection (first editor-descriptor)))))
  (let* ((elements (graph-selected-elements window))
         (parents ()))
    (loop for element in elements
          do (setf (getf parents (graph-element-parent element))
                     (union (cons element (when (typep element 'node-element) (node-element-links element)))
                            (getf parents (graph-element-parent element)))))
    (unless parents
      (return-from cg:delete-selection))
    (let ((command (make-command
                     :type 'delete-elements
                     :window window
                     :do-data (cons () (loop for values = parents then (rest (rest values))
                                             while values
                                             collect (list (first values) () (second values))))
                     :undo-data (cons elements (loop for values = parents then (rest (rest values))
                                                     while values
                                                     collect (list (first values) (second values) ())))
                     :label "Delete"
                     :do-function #'object-clipboard-command-do-function
                     :undo-function #'object-clipboard-command-undo-function)))
      (execute command))))

;;; We need a slightly more sophisticated deletion function to handle the
;;; way that parents are mapped. Generally speaking, too, we need the 
;;; undo data to record the positions of objects a bit. 

(defun command-delete-selection-do-function (command &aux (window (command-window command)))
  (setf (graph-elements window)
          (set-difference (graph-elements window) (command-do-data command)))
  (loop for element in (command-do-data command)
        do (refresh-graph window (graph-element-parent element) (graph-element-extent element))))

(defun command-delete-selection-undo-function (command &aux (window (command-window command)))
  (setf (graph-elements window)
          (append (graph-elements window) (command-undo-data command)))
  (loop for element in (command-do-data command)
        do (refresh-graph window (graph-element-parent element) (graph-element-extent element))))

