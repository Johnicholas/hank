;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University
;;;
;;; We need some kind of file system link. Basically, we need to be able to
;;; read files of Hank code into windows, and run with them. We'll want a file
;;; format that is reasonably open, and that ideally doesn't require us to use
;;; the Lisp reader and writer. This may mean that we need a buffering system 
;;; which we can use to manage the interface to files rather better than simply
;;; doing all the dispatching through write-char. 

(in-package "HANK")

;;; We should be a bit careful about how we choose to handle files at the bottom
;;; level. One possibility is to define our own file format. A second, rather
;;; simpler, idea, is that we use a simple format that is relatively open. After
;;; all, if we can edit the text, that makes life a lot easier. 
;;;
;;; File saving should be "safe"; that is, we should save the file to a temporary
;;; pathname, then we should rename the old file away and rename this file into 
;;; place, before deleting the old version. If we find the old version there, we
;;; can always read it in directly. It would also be really nice if we could use
;;; the same kind of code for handling the clipboard. 
;;;
;;; We also need the stuff that's used to create new files and to leave them without
;;; a pathname. This should be identical to the standard system for creating new 
;;; files. 
;;;
;;; So far so good. When we want to save a window, we need to implement the :save and
;;; the :save-as commands. Both will work fairly similarly. But first, let's look at
;;; the :new command. 

(defclass card-editor-window (editor-window)
  ())

(defmethod cg:default-pane-class ((window card-editor-window))
  'graph-window)

(comtab:defmenu-command :save ((window editor-window) &aux pathname)
  (if (setf pathname (editor-window-pathname window))
      (progn
        (record-command (application-window-main-window window) "Save")
        (save-editor-window window pathname))
      (:save-as window)))

(defconstant *save-formats* '((("Hank" . "*.hnk") "hnk")))

(comtab:defmenu-command :save-as ((window editor-window) &aux pathname)
  (let* ((old-pathname (editor-window-pathname window)))
    (multiple-value-bind (pathname format)
        (if *script-command*
            (let ((file (string-getf (compound-arguments *script-command*) "File")))
              (when file
                (let* ((pathname (pathname file))
                       (type (pathname-type pathname))
                       (format (position type *save-formats* :key #'second :test #'pro:whole-string-equal)))
                  (values pathname (or format 0)))))
            (cg:ask-user-for-new-pathname "Save Window As"
              :host (when old-pathname (directory-namestring old-pathname))
              :allowed-types (map 'list #'first *save-formats*)
              :initial-name (if old-pathname 
                                (file-namestring old-pathname)
                                (namestring
                                  (merge-pathnames (cg:stream-title window)
                                    (make-pathname :name "foo" 
                                                   :type (second (first *save-formats*))))))))
      (when pathname
        (unless (pathname-type pathname)
          (setf pathname
                  (make-pathname :type (second (elt *save-formats* format)) :defaults pathname)))
        (record-command (application-window-main-window window) 
          (make-compound "Save As" (list "File" (namestring pathname))))
        (save-editor-window window pathname)
      
        ;; Remove and then add the window, to make sure that it is correctly placed on
        ;; the window list, in order. This will move something to the end, so be warned
        ;; about that. 
       
        (let ((main-window (application-window-main-window window)))
          (remove-window window main-window)
          (add-window window main-window))
        (setf (editor-window-pathname window) pathname)))))

;;; We should be a bit smarter about the availability of these editor options. After
;;; all, they should not necessarily all be available all the time. We can use 
;;; commands for some of these. By default, a command will return nil if no window
;;; got through.
;;; 
;;; The first value returned by :saveablep says whether :save is enabled, and the
;;; second whether :save-as is enabled. 

(comtab:defmenu-command :saveablep ((window editor-window))
  (values t t))

(comtab:defmenu-command :printablep ((window editor-window))
  t)

(comtab:defmenu-command :exportablep ((window editor-window))
  (values t t))

;;; Finding out whether something is closable or not is a bit harder, to say the
;;; least. By the time the window appears, all that stuff's been written into
;;; the window frame type, and we can't get it again. We can take the same strategy
;;; and define all the windows that should be closable as such. 

(comtab:defmenu-command :closablep ((window editor-window))
  t)

(defmethod notify-window-selection ((window card-editor-window) move-to-front-p)
  (setf (getf (getf (cg:stream-plist (application-window-main-window window)) 'delayed-commands) 'set-cards-menu-available-p)
          (list move-to-front-p))
  (call-next-method))

(defun set-cards-menu-available-p (window value)
  (let* ((menu-bar (cg:window-menu window))
         (item (find-named-menu-item :cards menu-bar)))
    (cg:set-menu-item-available-p item value)))

;;; Above all, we need a textual representation for files. This will be the textual
;;; representation used underneath. Ideally, this will be supported by automatic
;;; layout rules which mean that a user's program can be read and written as text
;;; automatically. 

(defun get-stream-terms (stream)
  (let ((tokeniser (make-tokeniser stream)))
    (loop for term = (buffer-read-term tokeniser nil nil '#1=#:end-of-file)
          collect (progn
                    (when (eq term '#1#)
                      (loop-finish))
                    (let ((token (buffer-read-token tokeniser)))
                      (unless (eq token 'end)
                        (error "Syntax error")))
                    term))))

(defun get-file-terms (pathname)
  (with-open-file (stream pathname :direction :input :if-does-not-exist :error)
    (get-stream-terms stream)))

(defstruct (element-body (:type list))
  class
  properties
  shape
  template)

(defmacro element-body-body (list)
  `(rest (rest (rest (rest ,list)))))

;;; get-file-graph-elements can also return some options. These are stored in the
;;; system element, again, as a keyword value thing. We can use these values to
;;; try to put back the window where it was before. But we should be careful. If
;;; the window goes outside the screen, then we may not be able to resize it after
;;; we opened it. That is: files should be portable, if we can manage it, between
;;; different screens. 

(defun string-getf (plist key)
  (loop for list = plist then (rest (rest list))
        if (null list)
          do (return-from string-getf nil)
        end
        if (string-equal key (first list))
          do (return-from string-getf (second list))
        end))

(defun scale-to-screen (values)
  (loop for value in values
        collect (resolve value)))

(defun scale-to-internal (values)
  (loop for value in values
        collect (unresolve value)))

;;; We need to enhance this a bit, so that we can load links. These are added in
;;; as labels and list special forms, with a few extra properties. We can then
;;; turn these back into link elements in the display. To do this, we need to create
;;; all the elements before we create any of the links, and when we create the links
;;; we need to connect them to their elements properly. 

(defun get-graph-elements-from-forms (forms parent)
  (loop with graph-elements = nil
        with graph-element = nil
        for element in forms
        for element-name = (compound-operator element)
        for class = (element-body-class (compound-arguments element))
        for properties = (element-body-properties (compound-arguments element))
        for area = (scale-to-screen (string-getf properties "Area"))
        for columns = (scale-to-screen (string-getf properties "Columns"))
        for shape = (element-body-shape (compound-arguments element))
        for body = (element-body-body (compound-arguments element)) 
        do (setf graph-element (make-instance 'table-element
                                 :title element-name
                                 :class (intern (string-upcase class) "KEYWORD")
                                 :window nil
                                 :parent parent
                                 :column-widths columns
                                 :template (element-body-template (compound-arguments element))))
           (ecase (table-element-class graph-element)
             ((:fact_card)
              (setf (slot-value graph-element 'body-cells) (apply #'cg:make-position shape))
              (setf (slot-value graph-element 'row-data) (element-body-body (compound-arguments element))))
             ((:trace_box)
              (when (plusp (first shape))
                (setf (slot-value graph-element 'body-cells) (cg:make-position (first shape) 2)))
              (setf (slot-value graph-element 'row-data) (element-body-body (compound-arguments element))))
             ((:question_box)
              (when (plusp (first shape))
                (setf (slot-value graph-element 'body-cells) (cg:make-position (first shape) 1)))
              (setf (slot-value graph-element 'row-data) (element-body-body (compound-arguments element))))
             ((:instruction_card #|| :command_card :special_box ||#)
              (when (plusp (first shape))
                (setf (slot-value graph-element 'body-cells) (cg:make-position (first shape) 1)))
              (setf (slot-value graph-element 'body-size)
                      (apply #'cg:make-position (scale-to-screen (string-getf properties "BodySize"))))
              (setf (slot-value graph-element 'body-data) body)
              (setf (slot-value graph-element 'children)
                      (get-graph-element-body-from-forms (rest body) graph-element))))
           (if (eq (table-element-class graph-element) :trace_box)
               (progn
                 (apply #'set-workspace-element-area graph-element (string-getf properties "Trace Position"))
                 (setf (slot-value graph-element 'pendingp)
                         (pro:whole-string-equal (string-getf properties "Pending") "Yes"))
                 (let ((status (string-getf properties "Status")))
                   (when status
                     (setf (slot-value graph-element 'status) status)))
                 (let ((status-tab (string-getf properties "Status Tab")))
                   (when status-tab
                     (let ((status-element (make-instance 'status-tab-graph-element
                                             :window nil
                                             :parent parent
                                             :pendingp (slot-value graph-element 'pendingp)
                                             :status status-tab)))
                       (setf (graph-element-area status-element t) (tracer-get-status-box graph-element))
                       (push status-element graph-elements))))
                 (setf (slot-value graph-element 'column-widths) (divide-columns *trace-element-width* (first shape))))
               (progn
                 (setf (slot-value graph-element 'area) (apply #'cg:make-box area))
                 (setf (graph-element-area graph-element t)
                         (cg:copy-box (calculate-table-element-area graph-element)))))
           (push graph-element graph-elements)
        finally do (return (nreverse graph-elements))))

(defun get-graph-element-body-from-forms (body graph-element)
  (let ((index ()))
    (loop for values = body then (member-if #'stringp (rest values))
          if (null values)
            do (return)
          end
          do (setf index (list* (first values)
                           (list (first (get-graph-elements-from-forms (list (second values)) 
                                        graph-element))
                                 (let* ((commands (rest (rest values)))
                                        (end (position-if #'stringp commands)))
                                   (if end
                                       (subseq commands 0 end)
                                       commands)))
                           index)))
    (loop for keys = index then (rest (rest keys))
          for key = (first keys)
          for value = (second keys)
          if (null keys)
            do (loop-finish)
          end
          collect (first value)
          append (loop for form in (second value)
                       for connection = (make-connection
                                          (make-contact (first value) (string-getf (fourth form) "From"))
                                          (make-contact (first (string-getf index (third form))) (string-getf (fourth form) "To")))
                       for element = (make-instance 'link-element
                                       :label (second form)
                                       :connection connection
                                       :window (graph-element-window (first value))
                                       :parent (graph-element-parent (first value)))
                       do (push element (node-element-links (contact-element (connection-from connection))))
                          (push element (node-element-links (contact-element (connection-to connection))))
                       collect element))))

;;; After we've got the file terms, we may also get a file properties
;;; element. If we do, remove it from the list of terms and add it to
;;; the window's properties for post processing. 

(defun get-file-graph-elements (pathname parent)
  (let ((terms (get-file-terms pathname))
        (special-terms ()))
    (values
      (get-graph-elements-from-forms 
        (loop for term in terms
              for operator = (compound-operator term)
              if (and (> (length operator) 5)
                      (string-equal "Hank:" operator :end2 5)
                      (pro:whole-string-equal "File Properties"
                        (canonicalise-value (subseq operator 5))))
                do (push (cons :file-properties term) special-terms)
              else
                collect term)
        parent)
      (nreverse special-terms))))
    
;;; OK, we are now almost well enough armed to be able to write the open command. This
;;; should get all the graph elements, and then create a graph editor window 
;;; containing all these elements and with the appropriate pathname. 

(defmethod set-window ((element graph-element) graph)
  (setf (slot-value element 'window) graph)
  (loop for element in (slot-value element 'children)
        do (set-window element graph)))

(defmethod set-window ((element link-element) window)
  (call-next-method)
  (let* ((connection (link-element-connection element))
         (segment (create-segment))
         (from-point (contact-point (connection-from connection)))
         (to-point (contact-point (connection-to connection)))
         (from-position (nattachment-point-position 
                          (graph-element-area (contact-element (connection-from connection)))
                          '#.(cg:make-position 0 0) from-point))
         (to-position (nattachment-point-position 
                        (graph-element-area (contact-element (connection-to connection)))
                        '#.(cg:make-position 0 0) to-point)))
    (nmake-segment segment from-position from-point to-position to-point)
    (setf (slot-value element 'segment) (nthcdr (first segment) (rest (rest segment)))))
  (setf (graph-element-area element t)
          (cg:copy-box (calculate-link-element-area element))))

(defun install-file (pathname graph)
  (multiple-value-bind (elements special-terms) (get-file-graph-elements pathname graph)
    (loop for element in elements
          do (set-window element graph))
    (setf (graph-elements graph) elements)
    (setf (window-commands graph) nil)
    (cg:set-stream-prop graph 'special-terms special-terms)))

(comtab:defmenu-command :new ((main-window main-window))
  (record-command main-window "New")
  (let ((window (cg:open-stream 'card-editor-window (main-window-body main-window) :io
                  :main-window main-window
                  :window-state :shrunk)))
    (add-window window main-window)
    (let ((*record-select-window-p* nil))
      (declare (special *record-select-window-p*))
      (cg:select-window window))
    window))

;;; There's a few minor problems here. We should make sure that the file isn't already
;;; open. If it is, we should select that window again, rather than opening the file
;;; again in a new window. 
;;;
;;; Secondly, if the stream's visible name is the same as that of another file, we need
;;; to be careful about the stream name's for both. In fact, we won't worry too much about 
;;; that, but we'll use a specific ordering list, not the alphabetical ordering that is
;;; used by Allegro, where you've no idea which file your going to get at any given 
;;; time. 
;;;
;;; The page width and height matters.  They are used to store the number of pages that
;;; we're interested in. Printing might be a bit more problematic.  

(comtab:defmenu-command :open ((main-window main-window))
  (let ((pathname (or (when *script-command*
                        (let ((file (string-getf (compound-arguments *script-command*) "File")))
                          (and file
                               (probe-file file))))
                      (cg:ask-user-for-existing-pathname "File to Edit"
                        :allowed-types `(,@(map 'list #'first *save-formats*)
                                         ("All files" . "*.*"))))))
    (when pathname
      
      ;; We know that the file exists, but it might not be possible to read it. We should
      ;; try to read it all before we create the window, and then we won't get an empty 
      ;; window. We can then display the window appropriately.
       
      (loop for window in (all-windows main-window)
            do (when (and (typep window 'card-editor-window)
                          (equalp pathname (editor-window-pathname window)))
                 (cg:select-window window)
                 (return-from :open window)))
       
      (record-command main-window
        (make-compound "Open" (list "File" (namestring pathname))))
      
      (let* ((window (cg:open-stream 'card-editor-window (main-window-body main-window) :io
                       :main-window main-window 
                       :window-state :shrunk
                       :pathname pathname))
             (graph (editor-window-editor window)))
        (install-file pathname graph)
        (add-window window main-window)
         
        ;; Clear the cache of all interpreter elements. Note that there's no need to do
        ;; this for the :new command, because it doesn't add any elements. 
         
        (cg:set-stream-prop main-window 'interpreter-element-table :none)
         
        (let ((match (assoc :file-properties (cg:get-stream-prop graph 'special-terms)))
              (value ()))
          (when match
            (setf value (assoc "pages" (nthcdr 4 (compound-arguments (rest match)))
                          :test #'pro:whole-string-equal))
            (when value
              (assert (and (= (length (second value)) 2)
                           (every #'(lambda (value)
                                      (typep value '(integer 1 10)))
                                  (second value))))
              (setf (graph-pages graph) (apply #'cg:make-position (second value))))
            (setf value (assoc "interior" (nthcdr 4 (compound-arguments (rest match)))
                          :test #'pro:whole-string-equal))
            (when value
              (assert (and (= (length (second value)) 4)
                           (every #'integerp (second value))))
              (let ((box (apply #'cg:make-box (second value))))
                (when (cg:box-intersect-p box (cg:nvisible-box (main-window-body main-window)
                                                '#.(cg:make-box 0 0 0 0)))
                  (cg:reshape-window-interior window (apply #'cg:make-box (second value))))))))
         
        (let ((*record-select-window-p* nil))
          (declare (special *record-select-window-p*))
          (cg:select-window window))))))

;;; When we revert a file, we first want to throw out all the elements, then we 
;;; need to read all the file data again, and then finally we can redraw the
;;; window. In fact, we can simply ignore the old elements as they will be garbage
;;; collected at some later time. When we revert a file, we should set the
;;; command system so no more commands can be undone. 

(comtab:defmenu-command :revert ((window editor-window) &aux pathname)
  (setf pathname (editor-window-pathname window))
  (let ((graph (editor-window-editor window)))
    (install-file pathname graph)
    (setf (:modified-p window) nil)
    (cg:redisplay-window graph)))

;;; Saving a file does basically the opposite. This time we need to remember to keep the
;;; data in a form where we can generate the right system information. The first stage is
;;; to generate a set of Prolog terms which represent the file. Then we can sort out how
;;; to write that to the file with minimum fuss. 
;;;
;;; There's quite a lot more to it than this, because we need to keep data from tables
;;; which don't fit its size, but which should still be recorded. This data is kept for
;;; future resizing; data isn't generally lost when something is resized. Also, we need
;;; to manage textual representations of all the different kinds of object, not just
;;; static tables. Dynamic, question, and special tables are all just as important. 
;;; Two kinds of tables are different. Some tables are containers, other contain data. 
;;; Additional properties are stored in the __Hank definition. An additional complexity
;;; is that the data for elements should be managed to not display extra cells outside
;;; the element. These extra cells should be saved as extra data. Sometimes this can
;;; be quite big, but that shouldn't be a problem. 
;;;
;;; Elements are always sorted, in order, from the top left. The top left will always
;;; be stored for each element 

(defun sorted-file-elements (elements)
  (loop for element in (sort (loop for element in elements
                                   for area = (graph-element-area element)
                                   collect (list (+ (* (cg:box-left area) (cg:box-left area))
                                                    (* (cg:box-top area) (cg:box-top area)))
                                                 element))
                         #'< :key #'first)
        collect (second element)))

(defmethod get-file-element ((element t))
  nil)

(defmethod get-file-element ((element table-element))
  (let* ((title (table-element-title element))
         (area (graph-element-area element))
         (cells (grid-graph-element-cells element))
         (class (table-element-class element))
         (properties `("Area" ,(scale-to-internal
                                 (list (cg:box-left area) (cg:box-top area)
                                       (cg:box-right area) (cg:box-bottom area)))
                       "Columns" ,(scale-to-internal (table-element-column-widths element)))))
    (ecase class
      ((:trace_box)
       (make-compound title
         (list* (string-capitalize class)
                `("Trace Position" ,(getf (graph-element-properties element) 'trace-position)
                  "Pending" ,(if (slot-value element 'pendingp) "Yes" "No")
                  ,@(let ((status (slot-value element 'status)))
                      (when status `("Status" ,status)))
                  ,@(let ((status-element (getf (slot-value element 'properties) 'status-element)))
                      (when status-element `("Status Tab" ,(status-tab-graph-element-status status-element)))))
                (list (cg:position-x cells) (pro:i1- (cg:position-y cells)))
                (table-element-template element) (table-element-row-data element))))
      ((:fact_card :question_box)
       (make-compound title
         (list* (string-capitalize class)
                properties
                (if (eq class :fact_card)
                    (list (cg:position-x cells) (pro:i1- (cg:position-y cells)))
                    (list (cg:position-x cells)))
                (table-element-template element) (table-element-row-data element))))
      ((:instruction_card #|| :command_card :special_box ||#)
       (make-compound title
         (list* (string-capitalize class)
                (append properties
                        `("BodySize" ,(scale-to-internal
                                        (list (cg:position-x (table-element-body-size element))
                                              (cg:position-y (table-element-body-size element))))))
                (list (cg:position-x cells))
                (table-element-template element) 
                (first (table-element-body-data element))
                (get-body-file-elements
                  (sorted-file-elements (graph-elements element)))))))))

(defun get-file-elements (elements)
  (loop for element in (sorted-file-elements elements)
        for file-element = (get-file-element element)
        if file-element
          collect (list file-element element)))

(defun get-body-file-elements (body)
  (setf body (remove-if-not #'(lambda (element)
                                (typep element 'node-element))
                            body))
  (let ((index ()))
    (loop for label from 1 
          for element in body
          for value = (cl:format() "Label ~D" label)
          do (setf (getf index element) value))
    (loop for element in body
          collect (getf index element)
          append (loop for (element graph-element) in (get-file-elements (list element))
                       collect element)
          append (loop for link in (node-element-links element)
                       for connection = (link-element-connection link)
                       if (eq element (contact-element (connection-from connection)))
                         collect (list "If" (link-element-label link)
                                   (getf index (contact-element (connection-to connection)))
                                   (list "From" (contact-point (connection-from connection))
                                         "To" (contact-point (connection-to connection))))
                       end))))

(defun write-terms-to-stream (terms &optional (stream *standard-output*) &key crp)
  (let ((termer (make-termer stream)))
    (loop for term in terms
          do (buffer-write-term termer term :pretty t :escape t)
             (buffer-write-char termer #\.)
             (buffer-write-char termer #\Newline)
             (buffer-write-char termer #\Newline))
    (buffer-write-buffer termer)))

(defmethod save-editor-window ((window card-editor-window) pathname)
  (complete-element-editor (editor-window-editor window) :end)
  (with-open-file (output pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-terms-to-stream 
      (loop for (element graph-element) in (get-editor-file-elements window)
            collect element)
      output)
    (write-terms-to-stream (get-editor-special-terms window) output))
  (setf (:modified-p window) nil))

(defmethod get-editor-special-terms ((window card-editor-window))
  (list (make-compound "Hank: File Properties"
          `("Fact_Card"
            nil
            (2 1)
            ("Property" "Value")
            ("pages" ,(let ((pages (graph-pages (editor-window-editor window))))
                        (list (cg:position-x pages) (cg:position-y pages))))
            ("interior" ,(let ((box (cg:nwindow-interior window '#.(cg:make-box 0 0 0 0))))
                           (list (cg:box-left box) (cg:box-top box)
                                 (cg:box-right box) (cg:box-bottom box))))))))

