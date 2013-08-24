;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; HANK's control panel window. This window is global to the system,
;;; and is used to enter queries and to keep an eye on what HANK is up to
;;; at any given time. We don't want people to be able to close the
;;; control panel window. There may be a number of different system
;;; windows here. The control panel is probably the simplest, because it
;;; mainly manages the asking of questions and the getting of answers. Of
;;; course, people can also simply embed Hank: Ask tables into standard
;;; files. The control panel window is simply a place for people to write
;;; new queries and get them quickly evaluated. Output from the control
;;; panel window should appear in scrolling text below. 

(defclass system-graph-window (graph-window)
  ()
  (:default-initargs :auto-scroll-p nil))

(defmethod graph-drag-elements ((window system-graph-window) buttons position)
  nil)

(defmethod graph-select-event-mouse-handler ((window system-graph-window) event position buttons)
  nil)

(defclass system-window (application-window)
  ()
  (:default-initargs
   :user-scrollable nil))

(defmethod cg:user-close ((window system-window))
  (record-command (application-window-main-window window) "Close")
  (let ((parent (cg:stream-location window)))
    (cg:shrink-window window t)
    (notify-window-selection (cg:selected-window parent) t))
  t)

(defmethod cg:select-window ((window system-window) &optional recursivep)
  (let ((front-window (cg:selected-window (cg:stream-location window))))
    (unless (eq front-window window)
      (when front-window
        (notify-window-selection front-window nil))
      (when *record-select-window-p*
        (record-command (application-window-main-window window)
          (make-compound "Select Window"
            (list "Window" "Named" (cg:stream-title window)))))
      (notify-window-selection window t)
      ))
  (call-next-method))

(comtab:defmenu-command :closablep ((window system-window))
  t)

;;; Now that we've got system windows, we can build the control panel window
;;; itself. 

(defclass control-panel-window (system-window)
  ()
  (:default-initargs 
   :title "Control Panel"))

;;; When we build the control panel, we need to make sure that we've got enough room
;;; for a question box, with a little space around it. In fact, there will always be
;;; one question box in the control panel, a question box that cannot be deleted or
;;; moved, but which can be resized. We need to handle this with a little caution,
;;; therefore. 

(defmethod cg:device-open ((window control-panel-window) options)
  (prog1 (call-next-method)
    (let* ((box '#.(cg:make-box 0 0 0 0))
           (input-window-exterior '#.(cg:make-box 0 0 0 0))
           (button-size (cg:dialog-to-screen-units window
                          (cg:nmake-box '#.(cg:make-box 0 0 0 0) 0 0 45 10)))
           (button-panel-width (pro:i+ 10 10 (cg:box-right button-size)))
           (button-height (pro:i+ (cg:box-bottom button-size) 4 (cg:box-bottom button-size))))
      (cg:nvisible-box window box)
      (pro:idecf (cg:box-right box) button-panel-width)
      (let* ((element (make-instance 'table-element :class :question_box
                        :body-cells (cg:make-position 2 2)
                        :row-data (list (list "" ""))
                        :template (make-list 2 :initial-element "")
                        :column-widths (list *default-column-width* *default-column-width*)
                        :title "New Question"
                        :linkablep nil))
             (area (progn
                     (setf (slot-value element 'area) (cg:make-box 0 0 0 0))
                     (calculate-table-element-area element)))
             (graph-window (cg:open-stream 'system-graph-window window :io
                             :window-border :black
                             :user-scrollable nil
                             :window-state :shrunk
                             :right-attachment :right
                             :editor-window window
                             :page-lines-p nil
                             :window-interior (cg:nmake-box '#.(cg:make-box 0 0 0 0)
                                                0 0 (cg:box-width box) (pro:i+ *question-box-space* *question-box-space* 1 (cg:box-height area))))))
        (cg:set-stream-prop window 'input-window graph-window)
        (setf (graph-element-area element t)
                (cg:make-box *question-box-space* *question-box-space* (pro:i+ *question-box-space* (cg:box-width area)) (pro:i+ *question-box-space* (cg:box-height area))))
        (setf (slot-value element 'window) graph-window)
        (setf (slot-value element 'parent) graph-window)
        (setf (graph-elements graph-window) (list element))
        (setf (graph-element-selected-p element) t)
        (cg:select-window graph-window))

      (cg:nvisible-box window box)
      (nbox-inset box -1 -1)
      (pro:iincf (cg:box-top box) (cg:box-bottom (cg:nwindow-exterior (cg:get-stream-prop window 'input-window)
                                                   input-window-exterior)))
      (cg:set-stream-prop window 'output-window
        (cg:open-stream 'cg:text-edit-pane window :io
          :window-border :black
          :window-exterior box
          :font #+ACLPC (cg:make-font-ex :swiss nil 12)
                #+Procyon-Common-Lisp (cg:make-font :swiss nil 12)
          :user-scrollable :vertical
          :right-attachment :right
          :bottom-attachment :bottom))
    
      (cg:ncopy-box box input-window-exterior)
      (setf (cg:box-left box) (cg:box-right box))
      (setf (cg:box-right box) (pro:i+ (cg:box-left box) button-panel-width))
      (pro:idecf (cg:box-left box))
      (cg:set-stream-prop window 'dialog-window
        (cg:open-dialog (let ((left 10)
                              (right (pro:i- (cg:box-width box) 10))
                              (top (pro:i/ (pro:i- (cg:box-height box) button-height) 2)))
                          `(,(cg:make-dialog-item 
                               :widget 'cg:button
                               :title "Ask Briefly"
                               :set-value-fn #'(lambda (item new-value old-value)
                                                 (:ask-briefly
                                                   (cg:stream-location (cg:dialog-item-dialog item)))
                                                 nil)
                               :font *message-font*
                               :box (cg:nmake-box '#.(cg:make-box 0 0 0 0)
                                      left top right (pro:iincf top (cg:box-bottom button-size))))
                            ,@(progn (pro:iincf top 4) nil)
                            ,(cg:make-dialog-item 
                               :widget 'cg:button
                               :title "Ask in Full"
                               :set-value-fn #'(lambda (item new-value old-value)
                                                 (:ask-in-full
                                                   (cg:stream-location (cg:dialog-item-dialog item)))
                                                 nil)
                               :font *message-font*
                               :box (cg:nmake-box '#.(cg:make-box 0 0 0 0)
                                      left top right (pro:iincf top (cg:box-bottom button-size))))))
          'cg:dialog window
          :pop-up-p nil
          :background-color *dialog-background-colour*
          :window-border :black
          :window-exterior box
          :left-attachment :right
          :right-attachment :right)))
        
    (format (cg:get-stream-prop window 'output-window) "~A says: Hello~%" *character-name*)
    t))

;;; The other element, apart from the control panel, that we need to worry about is
;;; the trace window. This should look rather like the "comic strip" sheets, except
;;; that we'll need stepper buttons and stuff like that. The control panel is 
;;; strictly question -- answer, the comic strip trace window, when open, will go
;;; through everything between the two in more detail. Finally, if you select 
;;; something and do an "Ask", this will open the control panel, put the question
;;; into it, and then start to run the question. 
;;;
;;; The comic strip window needs some more elements, elements which are not shown
;;; in the main card window. These look a bit like question boxes, but they have 
;;; four rows rather than three. They will also need a status box, but that may
;;; have to wait for now. 

(defclass workspace-window (system-window)
  ()
  (:default-initargs 
   :title "Workspace"
   :user-scrollable nil))

(defun step-button-function (item new-value old-value)
  (:step (application-window-main-window 
           (frame-window-of-window (cg:stream-location (cg:dialog-item-dialog item)))))
  nil)

(defun run-button-function (item new-value old-value)
  (:run (application-window-main-window 
          (frame-window-of-window (cg:stream-location (cg:dialog-item-dialog item)))))
  nil)

(defun step-back-button-function (item new-value old-value)
  (:step-back (application-window-main-window 
                (frame-window-of-window (cg:stream-location (cg:dialog-item-dialog item)))))
  nil)

(defun run-back-button-function (item new-value old-value)
  (:run-back (application-window-main-window 
               (frame-window-of-window (cg:stream-location (cg:dialog-item-dialog item)))))
  nil)

(defun get-control-buttons ()
  (let* ((left 2)
         (top 2)
         (texture-info (get-button-texture-info))
         (width (cg:texture-info-width texture-info))
         (bottom (pro:i+ top (cg:texture-info-height texture-info))))
    (flet ((make-button (name function up down disabled)
             (cg:make-dialog-item :widget 'cg:pixmap-button
                                  :name name
                                  :box (cg:make-box left top (pro:iincf left width) bottom)
                                  :value nil
                                  :available-p nil
                                  :window-border :none
                                  :set-value-fn function
                                  :title (list (get-button-texture-info) up down disabled))))
      (list (make-button :run-back 'run-back-button-function fastleft.up fastleft.down fastleft.disabled)
            (make-button :step-back 'step-back-button-function left.up left.down left.disabled)
            (make-button :stop 'stop-button-function stop.up stop.down stop.disabled)
            (make-button :step 'step-button-function right.up right.down right.disabled)
            (make-button :run 'run-button-function fastright.up fastright.down fastright.disabled)
            ))))

;;; The workspace window now contains an extra item, the current step number.  This should be
;;; incremented every time something interesting happens, or decremented if the program is run
;;; backwards.  By noting the step number at the end of a trace, people can look at the length
;;; of the trace even when folding has happened.  

(defmethod cg:device-open ((window workspace-window) options)
  (prog1 (call-next-method window `(:window-state :shrunk ,@options))
    (let ((box '#.(cg:make-box 0 0 0 0)))
      (cg:nvisible-box window box)
      (pro:iincf (cg:box-top box) (+ 4 *palette-button-size*))
      (nbox-inset box -1 -1)
      (cg:set-stream-prop window 'output-window
        (cg:open-stream 'workspace-graph-window window :io
          :window-border :black
          :user-scrollable t
          :right-attachment :right
          :bottom-attachment :bottom
          :editor-window window
          :page-lines-p nil
          :window-exterior box))
      (cg:nvisible-box window box)
      (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) (+ 4 *palette-button-size*)))
      (pro:idecf (cg:box-bottom box))
      (let* ((buttons (get-control-buttons))
             (max-x (reduce #'max buttons :key #'(lambda (item)
                                                   (cg:box-right (cg:dialog-item-box item)))
                      :initial-value 0)))
        (cg:set-stream-prop window 'control-window
          (cg:open-dialog `(,@buttons ,(cg:make-dialog-item :widget 'cg:static-text
                                                            :font *message-font*
                                                            :box (cg:make-box (pro:i+ max-x 12) 6 (pro:i+ max-x 80) 26)
                                                            :value "Step number: ")
                                      ,(cg:make-dialog-item :widget 'cg:static-text
                                                            :font *message-font*
                                                            :name 'step-number
                                                            :box (cg:make-box (pro:i+ max-x 82) 6 (pro:i+ max-x 152) 26)
                                                            :value 0))
            'cg:dialog window
            :window-border :none
            :pop-up-p nil
            :window-state :normal
            :window-exterior box
            :background-texture cg:light-grey-texture
            :right-attachment :right)))
      (setf (cg:get-stream-prop window 'step-number) 
              (cg:find-named-object 'step-number (cg:get-stream-prop window 'control-window)))
      (let ((state (getf options :window-state :normal)))
        (cond ((eq state :maximized) (cg:zoom-window window :maximized))
              ((eq state :icon) (cg:shrink-window window nil))
              ((eq state :normal) (cg:select-window window)))))))

;;; We can also export the elements in the workspace. This works very similarly to
;;; the standard exporting. We can use this to embed comic strip things into
;;; documents, just like designs. Note that selection isn't enabled, so you can't
;;; export a selection. 

(comtab:defmenu-command :export ((window workspace-window) &aux pathname)
  (commence-export-elements window (graph-elements (cg:get-stream-prop window 'output-window)) "Export"))

(comtab:defmenu-command :printablep ((window workspace-window))
  (and (graph-elements (cg:get-stream-prop window 'output-window))
       (values t nil)))

(comtab:defmenu-command :exportablep ((window workspace-window))
  (and (graph-elements (cg:get-stream-prop window 'output-window))
       (values t nil)))

(defmethod export-pathname ((window workspace-window))
  nil)

(comtab:defmenu-command :clear-workspace ((window workspace-window))
  (let ((graph-window (cg:get-stream-prop window 'output-window))
        (control-window (cg:get-stream-prop window 'control-window)))
    (cg:set-stream-prop graph-window 'element-table nil)
    (setf (graph-elements graph-window) nil)
    (cg:invalidate-window graph-window)
    (loop for item in (cg:dialog-items control-window)
          when (typep item 'cg:pixmap-button)
          do (setf (cg:dialog-item-available-p item) nil)
             (setf (cg:dialog-item-value item) nil))
    (setf (cg:dialog-item-value (cg:get-stream-prop window 'step-number)) 0)))

(defclass workspace-graph-window (graph-window)
  ())

(defmethod graph-drag-elements ((window workspace-graph-window) buttons position)
  nil)

(defmethod graph-element-mouse-moved ((window workspace-graph-window) element part)
  (cg:set-cursor cg:*screen* cg:arrow-cursor))

;;; This might seem a bit sneaky, but this is the find definition code. The
;;; idea is that we augment the interpreter element table to record the graph
;;; element for a definition, and if ew're asked to open a selection we look 
;;; for a matching element and open it. 

(defmethod :open-selection ((window workspace-graph-window))
  (let ((selection (graph-selected-elements window)))
    (when (= (length selection) 1)
      (let* ((element (first selection))
             (title (getf (slot-value element 'properties) 'real-title))
             (columns (slot-value element 'template))
             (main-window (application-window-main-window (cg:stream-location window)))
             (cards (get-interpreter-element-table main-window))
             (match (assoc title cards :test #'pro:whole-string-equal)))
        (loop for card in (rest match)
              for environment = (match columns (first card) ())
              if (not (eq environment :fail))
                do (let* ((graph-element (third card))
                          (graph-window (graph-element-window graph-element))
                          (editor-window (frame-window-of-window graph-window)))
                     (loop for element in (graph-selected-elements graph-window)
                           if (not (eq element graph-element))
                             do (setf (graph-element-selected-p element) nil))
                     (unless (graph-element-selected-p graph-element)
                       (setf (graph-element-selected-p graph-element) t))
                     (cg:select-window editor-window)
                     (return-from :open-selection)))))))

(defmethod cg:redisplay-window ((window workspace-graph-window) 
                                &optional (box (cg:nvisible-box window '#.(cg:make-box 0 0 0 0))))
  (when (canvas-drawing-p window)
    (call-next-method)
    (redraw-workspace-grid window box)
    (redraw-graph window window box)))

;;; We add an extra element type, which is used when drawing control elements. It is actually
;;; very simple. It is simply drawn as a box containing a label. This is used to create the
;;; "chain" of elements drawn when executing an instruction card. At the end of the chain, the
;;; last element's status will be copied into the overall status. 

(defclass status-tab-graph-element (graph-element)
  ((status
     :initform ""
     :initarg :status
     :reader status-tab-graph-element-status)
   (pendingp
     :initform t
     :initarg :pendingp
     :reader status-tab-graph-element-pending-p)
   (class
     :initform :trace_box)))

(defmethod draw-graph-element ((element status-tab-graph-element) window redraw-box highlightp
                               &aux (area (graph-element-area element))
                                    (box '#.(cg:make-box 0 0 0 0)))
  (cg:ncopy-box box area)
  (cg:erase-contents-box window box)
  (ensure-grid-graphics-context window nil nil)
  (draw-table-element-part element window :status (slot-value element 'status)  box t)
  (ensure-grid-graphics-context window nil nil))

(defmethod cg:event ((window workspace-graph-window) (event (eql cg:virtual-key-down)) buttons data time)
  (cond ((pro:i= data machine:vk-left)
         (funcall (if (pro:ilogtest buttons cg:alt-key) #':run-back #':step-back)
           (application-window-main-window (cg:window-parent window))))
        ((pro:i= data machine:vk-right)
         (funcall (if (pro:ilogtest buttons cg:alt-key) #':run #':step)
           (application-window-main-window (cg:window-parent window))))
        ((pro:i= data machine:vk-space)
         (when (pro:ilogtest buttons cg:alt-key)
           (cg:event (cg:selected-window cg:*screen*) event buttons data time)))))