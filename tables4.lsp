;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University
;;;
;;; Even more spreadsheet interface stuff. The stuff here provides the spreadsheet
;;; drag editing system. This is implemented with a whole bunch of useful stuff. 

(in-package "HANK")

(defclass drag-table-resize (drag-boxes)
  ())

(defmethod graph-drag-resize-element ((window graph-window) element buttons position)
  (graph-drag-start-resize-element window element buttons position 'drag-table-resize))

(defmethod graph-drag-resize-header ((window graph-window) element buttons position)
  (graph-drag-start-resize-header window element buttons position 'drag-header-resize))
   
(defun graph-drag-start-resize-element (window element buttons position type)
  (loop for selected-element in (graph-selected-elements window)
        unless (eq element selected-element)
        do (setf (graph-element-selected-p selected-element) nil))
  (let* ((selected-graph-elements (graph-selected-elements window))
         (boxes (map 'list #'(lambda (graph-element)
                               (cg:copy-box (graph-element-area graph-element)))
                    selected-graph-elements))
         (drag (make-drag type window buttons (cg:copy-position position)
                 :boxes boxes
                 :items selected-graph-elements)))
      (drag-track drag)))

;;; Resizing a header is rather similar in terms of the objects that it refers to, and
;;; even in the pattern of dragging. The main difference is that the box approval process
;;; is a bit different and the command that is executed at the end is different, too. 

(defun drag-resize-track-draw-boxes (boxes window drawp)
  (declare (ignore drawp))
  (with-saved-paint-operation (window)
    (cg:set-paint-operation window #+Procyon-Common-Lisp cg:invert #+ACLPC cg:po-invert)
    (set-drag-line window)
    (unwind-protect (map nil #'(lambda (box)
                                 (cg:draw-box window box))
                         boxes)
      (unset-drag-line window)
      )))

(defmethod drag-hide-tracking ((drag drag-table-resize))
  (drag-resize-track-draw-boxes (drag-boxes-boxes drag) (slot-value drag 'source-window) nil))

(defmethod drag-show-tracking ((drag drag-table-resize))
  (drag-resize-track-draw-boxes (drag-boxes-boxes drag) (slot-value drag 'source-window) t))

(defmethod call-drag-event ((drag drag-table-resize) window event buttons data time)
  (declare (special cg::character))
  (let ((new-position '#.(cg:make-position 0 0))
        (old-position (cg:ncopy-position '#.(cg:make-position 0 0) (drag-current-position drag))))
    (if (cg:positionp data)
        (progn
          (cg:ncopy-position new-position data)
          (when (drag-scroll-if-required drag window data)
            (cg:ncursor-position window new-position)))
        (cg:ncursor-position window new-position))
    (when (or (pro:i= event cg:virtual-key-down)
              (pro:i= event cg:virtual-key-up)
              (pro:i= event cg::character)
              (pro:i= event cg:timer-event)
              (pro:i= event cg:mouse-moved)
              (cg:mouse-event-p event))
      
      ;; We should be a bit smarter than this. Instead of simply using positions, use
      ;; an adjusted "approved" box. This will be the best box approved by the new
      ;; position. Only if the box is different from the old box do we redraw anything. 
      ;; Again, this should use the "approved" box. The old box should always be 
      ;; approved. We can set this up when we start the drag. This will mean we're not
      ;; approving all the time. 
       
      (let ((old-box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (drag-table-resize-get-approved-box drag old-position)))
            (new-box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (drag-table-resize-get-approved-box drag new-position))))
        (unless (cg:box= old-box new-box)
          (drag-hide-tracking drag)
          (drag-move-tracking drag new-position old-position)
          (drag-show-tracking drag)
          (cg:ncopy-position (drag-current-position drag) new-position)))
      (drag-event drag window event buttons new-position data))))

(defmethod drag-table-resize-get-approved-box ((drag drag-table-resize) new-position)
  (let ((old-box (first (drag-boxes-boxes drag))))
    (ndrag-table-resize-approve-box (first (drag-items drag)) (cg:nmake-box '#.(cg:make-box 0 0 0 0)
                                                                (cg:box-left old-box) (cg:box-top old-box)
                                                                (cg:position-x new-position) (cg:position-y new-position))
      '#.(cg:make-position 0 0))))

;;; The header drag is a bit different, and needs a bit more care in the process
;;; by which we do the dragging. Note that a header drag can only apply to 
;;; dynamic and special tables. 

(defclass drag-header-resize (drag-table-resize)
  ())

;;; The size of the approved box depends on the object that we're resizing. For now,
;;; let's assume that we're dealing with full grid items. This is where all the smart
;;; stuff for the class of the element has to come in. 
;;;
;;; This function is hideous but necessary. 

(defun ndrag-table-resize-approve-box (element box size)
  (let* ((class (slot-value element 'class))
         (row-height (grid-graph-element-row-height element))
         (header (pro:i+ row-height *table-border-size*)))
    (case class
      ((:fact_card :question_box)
       (pro:iincf header row-height))
      ((:instruction_card)
       (pro:iincf header (pro:i+ row-height row-height))))
    (case class
      ((:fact_card)
       (setf (cg:box-bottom box)
               (pro:i+ (cg:box-top box) header
                       (pro:imax row-height
                                 (pro:i* (pro:i/ (pro:i+ (pro:i- (cg:box-bottom box) header (cg:box-top box))
                                                         (pro:i/ row-height 2))
                                                 row-height)
                                         row-height))))
       (setf (cg:position-y size) (pro:i/ (pro:i- (cg:box-bottom box) (cg:box-top box) header) row-height)))
      ((:question_box)
       (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) header row-height))
       (setf (cg:position-y size) (pro:i/ (pro:i- (cg:box-bottom box) (cg:box-top box) header) row-height)))
      ((:instruction_card #|| :command_card :special_box ||#)
       (setf (cg:box-bottom box)
               (pro:imax (cg:box-bottom box)
                         (pro:i+ (cg:box-top box)
                                 header
                                 (cg:position-y *minimum-body-size*))))
       (setf (cg:position-y size) (pro:i- (cg:box-bottom box) (cg:box-top box) header))))
    (case class
      ((:fact_card :question_box)
       (setf (cg:box-right box)
               (pro:i+ (cg:box-left box)
                       (let ((width (pro:i- (cg:box-right box) (cg:box-left box)))
                             (column-widths (slot-value element 'column-widths)))
                         (cond ((and (pro:i< width (pro:i/ (first column-widths) 2))
                                     (eq class :question))
                                (setf (cg:position-x size) 0)
                                (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) row-height *drag-header-resize-empty-box-size*))
                                (first column-widths))
                               ((pro:i< width (first column-widths))
                                (first column-widths))
                               (t
                                (loop with width-so-far = (first column-widths)
                                      for columns = (rest column-widths) then (rest columns)
                                      for column-number from 1
                                      for column-width = (or (first columns) *default-column-width*)
                                      if (pro:i< width (pro:i+ width-so-far (pro:i/ column-width 2)))
                                        do (setf (cg:position-x size) column-number)
                                           (return width-so-far)
                                      end
                                      do (pro:iincf width-so-far column-width))))))))
      #||
      ((:command_card :special_box)
       (setf (cg:box-right box)
                 (pro:imax (cg:box-right box)
                           (pro:i+ (cg:box-left box)
                                   (cg:position-x *minimum-body-size*))))
       (setf (cg:position-x size) (cg:box-width box)))
      ||#
      ((:instruction_card)
       (let ((cells (grid-graph-element-cells element))
             (column-widths (slot-value element 'column-widths)))
         (setf (cg:box-right box)
                 (pro:imax (cg:box-right box)
                           (pro:i+ (cg:box-left box)
                                   (pro:imax (cg:position-x *minimum-body-size*)
                                             (loop for column from 0 below (cg:position-x cells)
                                                   sum (or (nth column column-widths) *default-column-width*)))))))
       (setf (cg:position-x size) (cg:box-width box))))
    box))

;;; At the end of the drag, we need to work out the last number of rows and columns
;;; so we can resize the table. This is a bit similar to napprove-box, but remember,
;;; we really want to get the body rows and columns, and the body size, not the grid
;;; rows and columns or complete size.  

(defmethod drag-move-tracking ((drag drag-table-resize) new-position old-position)
  (let ((old-box (first (drag-boxes-boxes drag))))
    (cg:ncopy-box old-box (drag-table-resize-get-approved-box drag new-position))))

;;; This resizing is a bit too specific. In fact, we need to let people resize 
;;; things with and without a body, and these are rather different commands. 
;;; Beware that the stuff we get here depends on the class of object we're dealing
;;; with. 

(defmethod drag-drop-window ((drag drag-table-resize) (window graph) buttons position)
  (let* ((box (first (drag-boxes-boxes drag)))
         (element (first (drag-items drag)))
         (graph-window (graph-element-window element))
         (new-size '#.(cg:make-position 0 0)))
    (set-selection graph-window (list element) :deselectp t)
    (ndrag-table-resize-approve-box element box new-size)
    (:resize graph-window new-size)))

(defun drag-command-table-resize-do-function (command)
  (drag-command-table-resize-apply-function command (command-do-data command)))
(defun drag-command-table-resize-undo-function (command)
  (drag-command-table-resize-apply-function command (command-undo-data command)))

;;; Note that we need to adjust from the body cells to the grid cells before we can
;;; calculate the area. If necessary, make sure that the column widths are big enough
;;; for the new size. The size applying function will also depend on the class of
;;; item being processed. At least, though, we should now be getting the body size 
;;; required. 

(defun resize-container-element (element size)
  (cg:ncopy-position (slot-value element 'body-size) size)
  (let ((area (slot-value element 'area))
        (column-widths (slot-value element 'column-widths)))
    (setf (cg:box-right area) (pro:i+ (cg:box-left area)
                                      (loop for width in column-widths sum width))))
  (setf (graph-element-area element)
          (cg:copy-box (calculate-table-element-area element))))

(defun resize-table-element (element size)
  (let ((widths (slot-value element 'column-widths)))
    (cg:ncopy-position (slot-value element 'body-cells) size)
    (unless (pro:i>= (length widths) (cg:position-x size))
      (setf (slot-value element 'column-widths)
              (nconc widths (make-list (pro:i- (cg:position-x size) (length widths))
                              :initial-element *default-column-width*)))))
  (setf (graph-element-area element)
          (cg:copy-box (calculate-table-element-area element))))

(defun drag-command-table-resize-apply-function (command data)
  (let* ((element (first data))
         (size (second data)))
    (ecase (table-element-class element)
      ((:fact_card :question_box)
       (resize-table-element element size))
      ((:instruction_card #|| :command_card :special_box ||#)
       (resize-container-element element size)))))

;;; Various changes here to make dragging work for the header of a special or a 
;;; dynamic table. We probably need something similar for a question table. A
;;; question table may have no column headers, and may have no body as well. For
;;; this reason it probably needs header resizing as well. We probably shouldn't
;;; show header resize tabs unless the element is selected. 

(defun graph-drag-start-resize-header (window element buttons position type)
  (loop for selected-element in (graph-selected-elements window)
        unless (eq element selected-element)
        do (setf (graph-element-selected-p selected-element) nil))
  (let* ((selected-graph-elements (graph-selected-elements window))
         (boxes (map 'list #'(lambda (graph-element)
                               (let ((area (slot-value element 'area))
                                     (box '#.(cg:make-box 0 0 0 0))
                                     (cells (grid-graph-element-cells element)))
                                 (if (pro:izerop (cg:position-x cells))
                                     (let* ((template (slot-value graph-element 'template))
                                            (row-height (grid-graph-element-row-height element))
                                            (title-height (pro:i+ (if template 0 *table-border-size*) row-height)))
                                       (cg:nmake-box box 0 title-height *drag-header-resize-empty-box-size* (pro:i+ title-height *drag-header-resize-empty-box-size*)))
                                     (progn
                                       (cg:nmake-box box 0 0 (pro:i1- (cg:position-x cells)) (pro:i1- (cg:position-y cells)))
                                       (nmake-box-from-grid-box element box)))
                                 (cg:nbox-move box (cg:nmake-position '#.(cg:make-position 0 0)
                                                     (cg:box-left area) (cg:box-top area)))
                                 box))
                    selected-graph-elements))
         (drag (make-drag type window buttons (cg:copy-position position)
                 :boxes boxes
                 :items selected-graph-elements)))
      (drag-track drag)))

(defun ndrag-header-resize-approve-box (element box size)
  (let* ((class (slot-value element 'class))
         (row-height (grid-graph-element-row-height element))
         (header row-height))
    (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) (pro:i* row-height 2)))
    (setf (cg:box-right box)
            (pro:i+ (cg:box-left box)
                    (let* ((width (pro:i- (cg:box-right box) (cg:box-left box)))
                           (column-widths (slot-value element 'column-widths))
                           (first-column-width (first column-widths))) 
                      (cond ((pro:i< width (pro:i/ first-column-width 2))
                             (setf (cg:position-x size) 0)
                             (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) *drag-header-resize-empty-box-size*))
                             *drag-header-resize-empty-box-size*)
                            ((pro:i< width first-column-width)
                             (setf (cg:position-x size) 1)
                             (first column-widths))
                            (t
                             (loop with width-so-far = (first column-widths)
                                   for columns = (rest column-widths) then (rest columns)
                                   for column-number from 1
                                   for column-width = (or (first columns) *default-column-width*)
                                   if (pro:i< width (pro:i+ width-so-far (pro:i/ column-width 2)))
                                     do (setf (cg:position-x size) column-number)
                                        (return width-so-far)
                                   end
                                   do (pro:iincf width-so-far column-width)))))))
    box))

(defmethod drag-table-resize-get-approved-box ((drag drag-header-resize) new-position)
  (let ((old-box (first (drag-boxes-boxes drag))))
    (ndrag-header-resize-approve-box (first (drag-items drag)) (cg:nmake-box '#.(cg:make-box 0 0 0 0)
                                                                 (cg:box-left old-box) (cg:box-top old-box)
                                                                 (cg:position-x new-position) (cg:position-y new-position))
      '#.(cg:make-position 0 0))))

;;; Again, the header resizing command should always apply only to the selected
;;; element, and this time it's really easy because we only need one parameter,
;;; an integer width. 

(defun resize-header (window element columns)
  (let ((old-size '#.(cg:make-position 0 0))
        (command (make-command
                   :type 'resize-element-headers
                   :window window
                   :modify-window window
                   :label "Change Headers"
                   :do-function #'drag-command-header-resize-do-function
                   :undo-function #'drag-command-header-resize-undo-function)))
    (cg:ncopy-position old-size (table-element-body-cells element))
    (if (= (cg:position-x old-size) columns)
        (setf command nil)
        (progn
          (setf (command-do-data command) (list element columns))
          (setf (command-undo-data command) (list element (cg:position-x old-size)))))
    (when command
      (record-command (application-window-main-window (graph-editor-window window))
        (make-compound "Change Headers" (list columns)))
      (execute command))))

(comtab:defmenu-command :change-headers ((window graph-window) &aux class)
  (let ((command *script-command*))
    (when command
      (let ((values (compound-arguments command))
            (elements (graph-selected-elements window)))
        (unless (and (= (length elements) 1)
                     (typep (first elements) 'table-element)
                     (setf class (table-element-class (first elements)))
                     (or (eq class :instruction_card) 
                         #|| (eq class :command_card) 
                             (eq class :special_box) ||#))
          (cg:beep window)
          (return-from :change-headers))
        (resize-header window (first elements) (first values))))))

(defmethod drag-drop-window ((drag drag-header-resize) (window graph-window) buttons position)
  (let ((box (first (drag-boxes-boxes drag)))
        (element (first (drag-items drag)))
        (new-size '#.(cg:make-position 0 0)))
    (ndrag-header-resize-approve-box element box new-size)
    (resize-header window element (cg:position-x new-size))))

(defun drag-command-header-resize-do-function (command)
  (drag-command-header-resize-apply-function command (command-do-data command)))
(defun drag-command-header-resize-undo-function (command)
  (drag-command-header-resize-apply-function command (command-undo-data command)))

(defun drag-command-header-resize-apply-function (command data)
  (let* ((element (first data))
         (size (second data))
         (widths (slot-value element 'column-widths)))
    (setf (cg:position-x (slot-value element 'body-cells)) size)
    (unless (pro:i>= (length widths) size)
      (setf (slot-value element 'column-widths)
              (nconc widths (make-list (pro:i- size (length widths))
                              :initial-element *default-column-width*))))
    (let ((old-area (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (slot-value element 'area))))
      (setf (graph-element-area element)
              (cg:copy-box (calculate-table-element-area element)))
      (when (cg:box= old-area (slot-value element 'area))
        (refresh-graph (command-window command) (graph-element-parent element) (slot-value element 'extent))))))

;;; Added column resizing code.  This is mostly like the other procedures but the drawing is 
;;; different.  Implementation is generally considerably easier than many of the others, but
;;; it's still all rather tedious.  

(defun graph-drag-start-resize-column (window element buttons position column type)
  (loop for selected-element in (graph-selected-elements window)
        unless (eq element selected-element)
        do (setf (graph-element-selected-p selected-element) nil))
  (let* ((selected-graph-elements (graph-selected-elements window))
         (element (first selected-graph-elements))
         (cells (table-element-body-cells element))
         (box '#.(cg:make-box 0 0 0 0))
         (area (graph-element-area element)))
    (cg:nmake-box box column 0 column (cg:position-y cells))
    (nmake-box-from-grid-box element box)
    (cg:nbox-move box
      (cg:nmake-position '#.(cg:make-position 0 0) (cg:box-left area) (cg:box-top area)))
    (drag-track
      (make-drag type window buttons (cg:copy-position position)
        :line (list (cg:make-position (cg:box-left box) (pro:i+ 2 (cg:box-top box)))
                    (cg:make-position (cg:box-left box) (pro:i1- (cg:box-bottom box))))
        :element element
        :column column
        :column-width (elt (table-element-column-widths element) (pro:i1- column))
        :items selected-graph-elements))))

(defmethod graph-drag-resize-column ((window graph-window) element buttons position column)
  (graph-drag-start-resize-column window element buttons position column 'drag-column-resize))

(defclass drag-column-resize (drag-window-mixin drag-line)
  ((element
     :initarg :element
     :reader drag-column-resize-element)
   (column 
     :initarg :column
     :reader drag-column-resize-column)
   (column-width
     :initarg :column-width
     :accessor drag-column-resize-column-width)))

(defmethod drag-make-tracking ((drag drag-column-resize))
  (loop for box in (drag-line-line drag)
        collect (cg:copy-position box)))

(defmethod drag-copy-tracking ((drag drag-column-resize) new-tracking old-tracking)
  (loop for new-position in new-tracking
        for old-position in old-tracking
        do (cg:ncopy-position new-position old-position))
  new-tracking)

;;; The main rule for approval is that the column should be resized below a given minimum width,
;;; which should probably be expressed in a device independent form.  Unfortunately, this means
;;; we really need to know which column we're dealing with, and which element. 

(defmethod drag-get-approved-tracking ((drag drag-column-resize) new-tracking new-position)
  (let* ((element (drag-column-resize-element drag))
         (column (pro:i1- (drag-column-resize-column drag)))
         (box '#.(cg:make-box 0 0 0 0))
         (area (graph-element-area element)))
    (cg:nmake-box box column 0 column 0)
    (nmake-box-from-grid-box element box)
    (cg:nbox-move box
      (cg:nmake-position '#.(cg:make-position 0 0) (cg:box-left area) (cg:box-top area)))
    (let ((x (pro:imax (cg:position-x new-position) (pro:i+ *minimum-column-width* (cg:box-left box)))))
      (setf (cg:position-x (first new-tracking)) x)
      (setf (cg:position-x (second new-tracking)) x)
      (setf (drag-column-resize-column-width drag) (- x (cg:box-left box))))
    new-tracking))

(defmethod drag-tracking-equal ((drag drag-column-resize) tracking1 tracking2 &aux length)
  (and (loop for position1 in tracking1
             for position2 in tracking2
             if (not (cg:position= position1 position2))
               do (return nil)
             end
             finally do (return t))))

(defun drag-track-draw-column-header-tracking (tracking window drawp)
  (with-saved-paint-operation (window)
    (cg:set-paint-operation window equal)
    (cg:set-line-width window 2)
    (unwind-protect (cg:draw-line window (first tracking) (second tracking))
      (cg:set-line-width window 1)
      )))

(defmethod drag-show-tracking ((drag drag-column-resize))
  (let ((tracking (drag-old-tracking drag)))
    (drag-track-draw-column-header-tracking tracking (slot-value drag 'source-window) t)))

(defmethod drag-hide-tracking ((drag drag-column-resize))
  (let ((tracking (drag-old-tracking drag)))
    (drag-track-draw-column-header-tracking tracking (slot-value drag 'source-window) nil)))

;;; At the end of the gesture, we can generate a command to resize the column's width.  To do 
;;; this we first need to find out the width at the end of the drag.  

(defmethod drag-drop-window ((drag drag-column-resize) (window graph) buttons position)
  (let* ((element (first (drag-items drag)))
         (graph-window (graph-element-window element))
         (column (pro:i1- (drag-column-resize-column drag)))
         (old-column-width (elt (slot-value element 'column-widths) column))
         (new-column-width (drag-column-resize-column-width drag)))
    (execute (make-command
               :type 'resize-column-width
               :window graph-window
               :modify-window (graph-editor-window graph-window)
               :label "Change Column Width"
               :do-function #'drag-command-column-resize-do-function
               :undo-function #'drag-command-column-resize-undo-function
               :do-data (list element column new-column-width)
               :undo-data (list element column old-column-width)))))

;;; Now for the command to actually resize the element's column width.  This should, of course,
;;; be undoable.  This can then be called at the end of the drag gesture to change the element's
;;; column width for real.

(defun drag-command-column-resize-do-function (command)
  (drag-command-column-resize-apply-function command (command-do-data command)))
(defun drag-command-column-resize-undo-function (command)
  (drag-command-column-resize-apply-function command (command-undo-data command)))

(defun drag-command-column-resize-apply-function (command data)
  (destructuring-bind (element column width) data
    (let ((widths (slot-value element 'column-widths)))
      (setf (elt widths column) width)
      (let ((old-area (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (slot-value element 'area))))
        (setf (graph-element-area element)
                (cg:copy-box (calculate-table-element-area element)))
        (when (cg:box= old-area (slot-value element 'area))
          (refresh-graph (command-window command) (graph-element-parent element) (slot-value element 'extent)))))))

