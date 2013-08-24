;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; SNW: 24/3/97; This version of the graph system has been imported
;;; into HANK. We can use this to represent tables and all that sort of
;;; thing. We'll probably need to make fairly radical revision of it, in
;;; the end, but this makes a reasonable starting point. 
;;;
;;; For printing purposes, we store the paper size in pixels.  This should,
;;; generally speaking, be A4, and we don't yet handle people using other paper sizes.  
;;; We can then use this to print multiple pages when we need to.  
;;;
;;; The paper size can also be used to draw page boundary lines.  This would also be
;;; neat.  

(defclass graph ()
  ((elements
     :initform nil
     :initarg :elements
     :accessor graph-elements)
   (editor-window
     :initarg :editor-window
     :reader graph-editor-window)
   (pages
     :initform (cg:make-position 1 1)
     :reader graph-pages)
   (page-size
     :initform (cg:make-position (resolve a4-visible-width) (resolve a4-visible-height))
     :reader graph-page-size)
   (page-lines-p
     :initform t
     :initarg :page-lines-p
     :reader graph-page-lines-p)))

(defclass graph-window (graph canvas)
  ())

(defmethod graph-interactive-p ((graph graph-window))
  t)

(defmethod cg:redisplay-window ((window graph-window) &optional (box (cg:visible-box window)))
  (when (canvas-drawing-p window)
    (refresh-graph-background window box)
    (redraw-graph window window box)))

;;; The interior page size is changed (usually) by the disabled page setup menu.
;;; The number of pages can be changed, and should be made visible more or less
;;; immediately.
;;;
;;; Also, we need rather arbitrary limits on the page size; I'd suggest 10 by 10
;;; as being too big to use, but good enough for now.  Printing is going to be a 
;;; bit more of an issue, because we'll have to move all the right stuff onto the
;;; current page and hope we don't get screwed by someone making 5 page wide 
;;; instruction cards.

(defmethod (setf graph-pages) (value (graph graph))
  (cg:ncopy-position (slot-value graph 'pages) value)
  (cg:set-page-size graph
    (pro:i* (cg:position-x (graph-page-size graph)) (cg:position-x value))
    (pro:i* (cg:position-y (graph-page-size graph)) (cg:position-y value)))
   
  ;; Added to fix the lack of a scroll update.  
  (pc::update-scroll-bars-for-new-window-size graph)
  (cg:invalidate-window graph (cg:visible-box graph) t))

;;; Graphs contain nodes corresponding to elements and arcs which
;;; correspond to references between elements. These are all supported
;;; in the document structure. 
;;;
;;; These all contain elements which we need to manage. When elements 
;;; are added or deleted we maintain the cache, as we do when a property
;;; value is modified. The graph system assumes certain properties
;;; and can manage them accordingly. When these properties are not supplied,
;;; the graph system may need to default them in some sensible way. This
;;; should link to what will probably be an automatic layout algorithm
;;; of some kind. 
;;;
;;; Added the link slot. This can be used by the different types of
;;; graph element to store data related to their links. For a link, 
;;; this will typically contain the start and end graph elements, and
;;; for a node, it will typically contain a list of all links. 

(defclass graph-element ()
  ((area
    :initform (cg:make-box 0 0 0 0)
    :reader graph-element-area)
   (extent
    :initform (cg:make-box 0 0 0 0)
    :reader graph-element-extent)
   (selected-p
    :initform nil
    :reader graph-element-selected-p)
   (window
    :initarg :window
    :reader graph-element-window)
   (properties
    :initform nil
    :accessor graph-element-properties)
   (parent
    :initform nil
    :initarg :parent
    :reader graph-element-parent)
   (children
    :initform nil
    :accessor graph-element-children
    :accessor graph-elements)))

(defmethod container-sorted-objects ((element graph))
  (sorted-file-elements (graph-elements element)))
(defmethod container-objects ((element graph))
  (slot-value element 'elements))

(defmethod object-container ((element graph-element))
  (slot-value element 'parent))
(defmethod container-objects ((element graph-element))
  (slot-value element 'children))
(defmethod container-sorted-objects ((element graph-element))
  (sorted-file-elements (slot-value element 'children)))

(defmethod select-object ((element graph-element))
  (set-selection (graph-element-window element) (list element) :deselectp t))

(defstruct (contact (:constructor make-contact (element point))
                    (:type list))
  element
  point)
(defstruct (connection (:constructor make-connection (from to))
                       (:type list))
  from
  to)

(defclass node-element (graph-element)
  ((links
     :initform nil
     :accessor node-element-links)))

(defclass link-element (graph-element)
  ((segment
     :initarg :segment
     :accessor link-element-segment)
   (label
     :initform "OK"
     :initarg :label
     :reader link-element-label)
   (label-size
     :initform nil)
   (connection
     :initarg :connection
     :reader link-element-connection)))

;;; An important aspect of graphs is layout. When there is no property to show
;;; where an element should be drawn, we need to derive a place. This should
;;; actually apply to a subset of elements, so that we can do sensible placement
;;; as much as possible. The actual layout algorithm, though, should be something
;;; which can be chosen by the document rather than being defined completely by
;;; the system for all different classes. This allows icon layout to be different
;;; from graph layout. 
;;;
;;; Clipping should really be handled more carefully. While we clip to the box
;;; in redraw, we should also clip the text. We can do this, but with 
;;; with-clipping-box because it isn't nestable. We add a new macro 
;;; which can be used but only within with-clipping-box, which is nestable.
;;; redraw-clipped-graph may be recursive!

(defmethod graph-element-interior (element)
  (graph-element-area element))

(defun redraw-clipped-graph (window parent box)
  (let ((clipping-box (cg:copy-box (graph-element-interior parent))))
    (nbox-intersect clipping-box box)
    (with-clipping-box (window clipping-box)
      (redraw-graph window parent clipping-box))))

(defun refresh-clipped-graph (window parent box)
  (let ((clipping-box (cg:copy-box (graph-element-interior parent))))
    (nbox-intersect clipping-box box)
    (with-clipping-box (window clipping-box)
      (refresh-graph window parent clipping-box))))

;;; Note the ordering implicit here: we are drawing the last element
;;; first in the refresh. We should really pass in the graph box intersection,
;;; which we can then use to allow the graph element to choose which parts of 
;;; itself it really does need to redraw. This may well not be all of it. 
;;; All elements should be drawn "solid", so that if they are covered 
;;; completely by something else, within the box, we shouldn't need to 
;;; redraw them at all. If they are not covered completely by something else,
;;; we need to redraw the box that isn't covered. Unfortunately, we can't calculate
;;; this box in CG, so we should be a bit more careful. 
;;;
;;; redraw-graph may be called recursively, so we need to be a bit careful about
;;; our handling of boxes. 

(defun redraw-graph (window parent box)
  (loop with pending-element = nil
        with pending-box = (cg:make-box 0 0 0 0)
        with extent-box = (cg:make-box 0 0 0 0)
        with pending-selected-p = nil
        for element in (graph-elements parent)
        do (cg:ncopy-box extent-box (slot-value element 'extent))
           (when (and pending-element (not (cg:sub-box-p pending-box (slot-value element 'area))))
             (draw-graph-element pending-element window pending-box pending-selected-p)
             (setf pending-element nil))
           (when (cg:box-intersect-p extent-box box)
             (nbox-intersect extent-box box)
             (cg:ncopy-box pending-box extent-box)
             (setf pending-element element)
             (setf pending-selected-p (graph-element-selected-p element)))
        finally do
            (when pending-element
              (draw-graph-element pending-element window pending-box pending-selected-p))))

;;; Areas outside the page should be drawn grey, to show people they shouldn't be touching
;;; things there.  

(defun refresh-graph-background (window box)
  (unless (graph-page-lines-p window)
    (cg:erase-contents-box window box)
    (return-from refresh-graph-background nil))
  (with-saved-foreground-colour (window)
    (let* ((page-size (graph-page-size window))
           (page-width (cg:position-x page-size))
           (page-height (cg:position-y page-size))
           (pages (graph-pages window))
           (min-x (pro:i/ (cg:box-left box) page-width))
           (max-x (pro:i/ (pro:i+ (cg:box-right box) (pro:i1- page-width)) page-width))
           (min-y (pro:i/ (cg:box-top box) page-height))
           (max-y (pro:i/ (pro:i+ (cg:box-bottom box) (pro:i1- page-height)) page-height))
           (start '#.(cg:make-position 0 0))
           (end '#.(cg:make-position 0 0))
           (limit-x (pro:i* page-width (cg:position-x pages)))
           (limit-y (pro:i* page-height (cg:position-y pages)))
           (erase-box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) box)))
      (setf (cg:box-right erase-box) (pro:imin (cg:box-right erase-box) limit-x))
      (setf (cg:box-bottom erase-box) (pro:imin (cg:box-bottom erase-box) limit-y))
      (cg:erase-contents-box window erase-box)
      (when (and #|| (pro:i> limit-x (cg:box-left box)) ||# (pro:i< limit-x (cg:box-right box)))
        (cg:set-foreground-colour window *header-colour*)
        (cg:fill-box window
          (cg:nmake-box '#.(cg:make-box 0 0 0 0) limit-x (cg:box-top box) (cg:box-right box) (cg:box-bottom box))))
      (when (and #|| (pro:i> limit-y (cg:box-top box)) ||# (pro:i< limit-y (cg:box-bottom box)))
        (cg:set-foreground-colour window *header-colour*)
        (cg:fill-box window
          (cg:nmake-box '#.(cg:make-box 0 0 0 0) (cg:box-left box) limit-y (cg:box-right box) (cg:box-bottom box))))
      (cg:set-foreground-colour window *title-colour*)
      (cg:nmake-position start 0 (cg:box-top box))
      (cg:nmake-position end 0 (cg:box-bottom box))
      (loop for x from (pro:imax 1 min-x) below (pro:imin max-x (pro:i1+ (cg:position-x pages)))
            do (setf (cg:position-x start) (pro:i* x page-width))
               (setf (cg:position-x end) (pro:i* x page-width))
               (cg:draw-line window start end))
      (cg:nmake-position start (cg:box-left box) 0)
      (cg:nmake-position end (cg:box-right box) 0)
      (loop for y from (pro:imax 1 min-y) below (pro:imin max-y (pro:i1+ (cg:position-y pages)))
            do (setf (cg:position-y start) (pro:i* y page-height))
               (setf (cg:position-y end) (pro:i* y page-height))
               (cg:draw-line window start end)))))

(defun refresh-graph (window parent box)
  (refresh-graph-background window box)
  (redraw-graph window parent box))

(defmethod draw-graph-element (element window box highlightp)
  (cg:draw-box window (graph-element-area element)))

(defmethod highlight-graph-element (element window highlightp old-highlightp)
  (let ((extent (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-extent element))))
    (refresh-graph-background window extent)
    (draw-graph-element element window extent highlightp)))

(defmethod (setf graph-element-selected-p) (value (graph-element graph-element) &optional recursivep
                                            &aux old-value)
  (when recursivep
    (setf (slot-value graph-element 'selected-p) value)
    (return-from graph-element-selected-p))
  (unless (eql value (setf old-value (graph-element-selected-p graph-element)))
    (setf (graph-element-selected-p graph-element t) value)
    (let ((parent (graph-element-parent graph-element))
          (window (graph-element-window graph-element)))
      (when value
        (setf (graph-elements parent)
                (nconc (delete graph-element (graph-elements parent)) (list graph-element))))
      (when (member (cg:window-state window) '(:normal :maximized))
        (let ((extent (slot-value graph-element 'extent)))
          (refresh-clipped-graph window parent extent)))))
  value)

;;; When we change the area of an element, find the difference and use
;;; that to recursively adjust all the children. This should probably happen
;;; recursively, for safety. By the way, the delta can structure share here
;;; because the delta will be constant during the recursive move! 
;;;
;;; Note that redraw-clipped-graph may be recursive. Not everything here is
;;; working just as it should. 
;;; 
;;; A neat solution to some nasty problems involving the movement of items within
;;; an element. If an element is moved partly outside its parent, the area of the
;;; parent can be adjusted. If the drag is to move the element completely outside
;;; its parent, then a bigger drag idea is probably required. 

(defmethod (setf graph-element-area) (value (graph-element graph-element) &optional recursivep
                                      &aux (old-value '#.(cg:make-box 0 0 0 0)))
  (when recursivep
    (let* ((old-area (slot-value graph-element 'area))
           (delta (cg:nmake-position '#.(cg:make-position 0 0)
                    (pro:i- (cg:box-left value) (cg:box-left old-area))
                    (pro:i- (cg:box-top value) (cg:box-top old-area)))))
      (loop for element in (graph-elements graph-element)
            if (typep element 'node-element)
              do (let* ((area (slot-value element 'area))
                        (new-area (cg:copy-box area)))
                   (cg:nbox-move new-area delta)
                   (setf (graph-element-area element t) new-area)))
      (loop for element in (graph-elements graph-element)
            if (and (graph-element-window element)
                    (typep element 'link-element))
              do (prepare-to-update-link-area element)
                 (setf (graph-element-area element t)
                         (cg:copy-box (calculate-link-element-area element)))))
    (cg:ncopy-box (slot-value graph-element 'area) value)
    (calculate-graph-element-extent graph-element)
    (return-from graph-element-area))
  (unless (cg:box= value (cg:ncopy-box old-value (slot-value graph-element 'area)))
    (let ((old-extent (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (slot-value graph-element 'extent)))
          (window (graph-element-window graph-element))
          (parent (graph-element-parent graph-element))
          (extent '#.(cg:make-box 0 0 0 0)))
      (setf (graph-element-area graph-element t) value)
      (cg:ncopy-box extent (slot-value graph-element 'extent))
      (when (member (cg:window-state window) '(:normal :maximized))
        (if (cg:box-intersect-p extent old-extent)
            (progn
              (nbox-union old-extent extent)
              (refresh-graph-background window old-extent)
              (redraw-clipped-graph window window old-extent)
              )
            (progn
              (refresh-graph-background window old-extent)
              (redraw-clipped-graph window window old-extent)
              (refresh-graph-background window extent)
              (redraw-clipped-graph window window extent)
              )))))
  value)

;;; Elements which are nodes should calculate their extent from 
;;; their area property. We also need to copy their area property
;;; from the element to the graph element. The area property can then
;;; be used for the display. 

(defmethod calculate-graph-element-extent (graph-element)
  (let* ((area (graph-element-area graph-element))
         (extent (cg:ncopy-box '#.(cg:make-box 0 0 0 0) area)))
    (nbox-inset extent #.(- extent-slop) #.(- extent-slop))
    (cg:ncopy-box (slot-value graph-element 'extent) extent)))

;;; The drawing function for the document will usually map the element to
;;; a set of drawing functions on the window. We do not define how it does
;;; that particularly precisely. We do not even, at this stage, define the
;;; difference between nodes and edges. 
;;;
;;; When we get a set of elements being added or deleted, or even moved, we
;;; need to remove them from one place on the window and arrange their redrawing.
;;; Often we will do this by invalidating the extent of a set of objects but
;;; before we do this we need to make sure that we have ensured all the extents.

(defun nelements-extent (box elements)
  (let ((found nil))
    (loop for element in elements
          for extent = (graph-element-extent element)
          do (if found
                 (nbox-union box extent)
                 (progn 
                   (setf found t)
                   (cg:ncopy-box box extent))))
    (when found
      box)))

(defun redisplay-elements-extent (window elements)
  (let ((box (nelements-extent '#.(cg:make-box 0 0 0 0) elements)))
    (when box
      (refresh-graph-background window box)
      (redraw-clipped-graph window box))))

(defun redraw-elements-extent (window elements)
  (let ((box (nelements-extent '#.(cg:make-box 0 0 0 0) elements)))
    (when box
      (redraw-clipped-graph window box))))

;;; By default, everything is in the part t when the left button is 
;;; down. When the right button is down, we look for a close attachment
;;; point and take that, or nil, as the part. 
;;;
;;; This function needs to be augmented so that links don't get handled
;;; as if they were simple boxes. This is required for a large part of
;;; the interaction handling code. We can also map this into some fancier
;;; cursor stuff, if we want. 

(defmethod graph-element-part (element position)
  t)

;;; For now, anything that isn't in an element has the part code nil, which
;;; won't do anything. 

(defmethod graph-element-part ((element null) position)
  (declare (ignore element position))
  nil)

;;; The suggested operation can be derived from the part, and the 
;;; buttons and all the other information associated with the element
;;; event. 
;;;
;;; The list of selected elements often needs to be sanitised to deal with implicit
;;; ancestry.

(defun graph-selected-elements (window)
  (loop for element in (graph-elements window)
        if (graph-element-selected-p element)
          append (list element)
        end
        append (graph-selected-elements element)))

(defun ancestorp (window element1 element2)
  (loop with e1 = element1
        for e2 = element2 then (graph-element-parent e2)
        do (cond ((eq e1 e2) (return t))
                 ((eq e2 window) (return nil)))))

(defun remove-descendants (window elements)
  (loop with result = ()
        for element in elements
        do (loop for old-element in result
                 do (cond ((ancestorp window old-element element)
                           (return nil))
                          ((ancestorp window element old-element)
                           (setf result (nsubstitute element old-element result))
                           (return nil)))
                 finally do (push element result))
        finally do (return result)))

;;; A few more compensations for the link into a hierarchical element 
;;; system.

(defmethod graph-element-area ((window graph))
  (cg:nvisible-box window '#.(cg:make-box 0 0 0 0)))
(defmethod graph-element-extent ((window graph))
  (cg:nvisible-box window '#.(cg:make-box 0 0 0 0)))

;;; A constant table of attachment points which are arranged around a
;;; box and not inside the box. 
;;;
;;; Changed attachment point coding. We only need four attachment 
;;; points, so we remove those at the corners. This also means that
;;; we always have a handle within the node graphics, and so 
;;; redrawing a node is enough to remove handles. 

(defconstant attachment-point-table
  '((#b00000000100000 . "Left")
    (#b01000000000000 . "Top")
    (#b10000000100000 . "Right")
    (#b01000001000000 . "Bottom")))

(defconstant attachment-points
  (map 'vector #'first attachment-point-table))

(defun nattachment-point-position (box position point)
  (let* ((left (cg:box-left box))
         (top (cg:box-top box))
         (width (pro:i- (cg:box-right box) left))
         (height (pro:i- (cg:box-bottom box) top))
         (horizontal (pro:i/ point #x80))
         (vertical (pro:ilogand point #x7F)))
    (cg:nmake-position position
      (pro:i+ left (pro:i/ (pro:i* horizontal width) #x40))
      (pro:i+ top (pro:i/ (pro:i* vertical height) #x40)))))

(defmacro scale-attachment-point (value)
  `(round (* ,value #.(expt 2 *attachment-point-word-bits*))))

(defun make-attachment-point (x y)
  (+ (ash (scale-attachment-point x) *attachment-point-word-size*)
     (scale-attachment-point y)))

(defun attachment-point (area position)
  (let* ((left (cg:box-left area))
         (top (cg:box-top area))
         (proportion-x (max 0 (min 1 (/ (- (cg:position-x position) left)
                                        (- (cg:box-right area) left)))))
         (proportion-y (max 0 (min 1 (/ (- (cg:position-y position) top)
                                        (- (cg:box-bottom area) top)))))
         (dx (min proportion-x (- 1 proportion-x)))
         (dy (min proportion-y (- 1 proportion-y))))
    (if (< dx dy)
        (make-attachment-point (if (= dx proportion-x) 0 1) proportion-y)
        (make-attachment-point proportion-x (if (= dy proportion-y) 0 1)))))

;;; The use of mod here is a trick. Because both 64 and 0 represent end codes,
;;; we can test for an end code using zerop of mod. We don't need to ever to
;;; a real subtraction. 

(defun attachment-point-nsignum (point position)
  (let* ((x (ldb (byte *attachment-point-word-size* *attachment-point-word-size*) point))
         (y (ldb (byte *attachment-point-word-size* 0) point))
         (dxp (zerop (mod x #.(expt 2 *attachment-point-word-bits*))))
         (dyp (zerop (mod y #.(expt 2 *attachment-point-word-bits*)))))
    (if dxp
        (cg:nmake-position position (if (zerop x) -1 1) 0)
        (cg:nmake-position position 0 (if (zerop y) -1 1)))))

(comtab:defmenu-command :command-window ((window graph))
  (let ((editor-descriptor (getf (cg:stream-plist window) 'element-editor)))
    (when editor-descriptor
      (return-from :command-window (:command-window (first editor-descriptor)))))
  (call-next-method))

(comtab:defmenu-command :rename ((window graph-window) new-value &aux element old-value)
  (let ((command *script-command*))
    (when command
      (setf new-value (first (compound-arguments command)))))
  (setf element (first (graph-selected-elements window)))
  (setf new-value (canonicalise-value new-value))
  (setf old-value (table-element-title element))
  (unless (string= (write-term-to-string old-value) new-value)
    (let ((command (make-command
                     :type 'rename-table
                     :window window
                     :modify-window (graph-editor-window window)
                     :label "Rename"
                     :do-data (list element :title new-value)
                     :undo-data (list element :title old-value)
                     :do-function #'element-edit-title-command-do-function
                     :undo-function #'element-edit-title-command-undo-function)))
      (record-command (application-window-main-window (graph-editor-window window))
        (make-compound "Rename" (list new-value)))
      (execute command))))

(comtab:defmenu-command :edit ((window graph-window) part new-value &aux element old-value)
  (setf element (first (graph-selected-elements window)))
  (let ((command *script-command*))
    (when command
      (setf new-value (second (compound-arguments command)))
      (setf part (first (compound-arguments command)))
      (cond ((listp part)
             (setf part (apply #'cg:make-position part)))
            (t
             (error "Invalid part to edit: ~A" part)))))
  (setf old-value (grid-graph-element-cell-data element part))
  (setf new-value (canonicalise-value new-value))
  (unless (string= (write-term-to-string old-value) new-value)
    (let ((command (make-command
                     :type 'edit-cell
                     :window window
                     :modify-window (graph-editor-window window)
                     :label "Edit"
                     :do-data (list element part new-value)
                     :undo-data (list element part old-value)
                     :do-function #'element-edit-cell-command-do-function
                     :undo-function #'element-edit-cell-command-undo-function)))
      (record-command (application-window-main-window (graph-editor-window window))
        (make-compound "Edit" (list (etypecase part 
                                      (cg:position (list (cg:position-x part) (cg:position-y part))))
                                    new-value)))
      (execute command))))

(defun modify-if (condition function value)
  (if condition
      (funcall function value)
      value))

(comtab:defmenu-command :resize ((window graph-window) new-value &aux element old-value scalep)
  (setf element (first (graph-selected-elements window)))
  (setf old-value '#.(cg:make-position 0 0))
  (ecase (table-element-class element)
    ((:instruction_card #|| :command_card :special_box ||#)
     (setf scalep t)
     (cg:ncopy-position old-value (table-element-body-size element)))
    ((:fact_card :question_box)
     (setf scalep nil)
     (cg:ncopy-position old-value (table-element-body-cells element))))
  (let ((command *script-command*))
    (when command
      (setf new-value (apply #'cg:make-position (modify-if scalep #'scale-to-screen (first (compound-arguments command)))))))
  (let ((command (make-command
                   :type 'resize-table
                   :window window
                   :modify-window (graph-editor-window window)
                   :label "Resize"
                   :do-function #'drag-command-table-resize-do-function
                   :undo-function #'drag-command-table-resize-undo-function)))
    (if (cg:position= new-value old-value)
        (setf command nil)
        (progn
          (setf (command-do-data command) (list element (cg:copy-position new-value)))
          (setf (command-undo-data command) (list element (cg:copy-position old-value)))))
    (when command
      (record-command (application-window-main-window (graph-editor-window window))
        (make-compound "Resize"
          (list (modify-if scalep #'scale-to-internal (list (cg:position-x new-value) (cg:position-y new-value))))))
      (execute command))))

