;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

;;; We now have a bit more work to do. What we need now is to be able to click on
;;; a table cell and for that to create a temporary text editor within that window, or at 
;;; least apparently within that window. This might be a bit hard, but it shouldn't be.
;;; We won't need it to scroll or anything, because as soon as we try scroll the 
;;; small editor should be hidden. For now all we need to do is to create the text
;;; editor when there is a click on a cell in the spreadsheet. It would be nice if we
;;; could clean up drag handling at the same time, but that's not terrifically easy. 
;;;
;;; Again as a special case, dynamic and special tables have two resize tabs, one for
;;; the headers and one for the body. Both need to be handled. Again, add a bit of
;;; a hack. If the mouse is in a header row, we should make sure that this shows up
;;; as a different part. This is really only needed for dragging and for mouse stuff. 
;;; The attachment point system has changed, so move on with the new linking system
;;; in the part system. 

(defmethod graph-element-part ((element table-element) point)
  (unless (and (graph-element-selected-p element)
               (null (rest (graph-selected-elements (graph-element-window element)))))
    (return-from graph-element-part t))
  (let* ((area (graph-element-area element))
         (class (slot-value element 'class))
         (top-level-p (typep (graph-element-parent element) 'cg:window))
         (box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) area))
         (position (cg:nmake-position '#.(cg:make-position 0 0)
                     (pro:i- (cg:position-x point) (cg:box-left area))
                     (pro:i- (cg:position-y point) (cg:box-top area)))))
     
    (unless (element-type-selectable-p (assoc class *element-types*))
      (return-from graph-element-part t))
     
    (setf (cg:box-left box) (pro:i- (cg:box-right box) *resize-box-size*))
    (setf (cg:box-top box) (pro:i- (cg:box-bottom box) *resize-box-size*))
    (nbox-inset box -2 -2)
    (when (cg:inside-box-p point box)
      (return-from graph-element-part :resize))
     
    (cg:ncopy-box box area)
     
    (unless (or top-level-p (not (table-element-linkable-p element)))
      (nbox-inset box *attachment-point-scope* *attachment-point-scope*)
      (unless (cg:inside-box-p point box)
        (return-from graph-element-part :link)))
     
    (when (and (cg:inside-box-p point area)
               (pro:i<= (cg:position-y position) (grid-graph-element-row-height element)))
      (return-from graph-element-part :title))
    (when (containerp class)
      (ntable-element-header-resize-box box element area)
      (nbox-inset box -2 -2)
      (when (cg:inside-box-p point box)
        (return-from graph-element-part :resize-header)))
     
    ;; Extra handling needed for some new parts.  These are represented as fixnums, rather than
    ;; positions, and refer to the column resize tabs.  Column resize tabs require us to be in
    ;; the header, and close to a header line.
     
    (let ((result (nget-point-cell element position)))
      (when (and result (zerop (cg:position-y position)))
        (let ((box '#.(cg:make-box 0 0 0 0))
              (x (pro:i- (cg:position-x point) (cg:box-left area))))
          (cg:nmake-box-from-corners box result result)
          (nmake-box-from-grid-box element box)
          (cond ((and (pro:iplusp (cg:position-x position)) (pro:i< (pro:iabs (pro:i- x (cg:box-left box))) 4))
                 (return-from graph-element-part (cg:position-x result)))
                ((pro:i< (pro:iabs (pro:i- x (cg:box-right box))) 4)
                 (return-from graph-element-part (pro:i1+ (cg:position-x result)))))))
     
      (return-from graph-element-part
        (or result
            (if (containerp class)
                (progn
                  (setf (cg:position-x position) (pro:i1+ (cg:box-left area)))
                  (let ((cell (nget-point-cell element position)))
                    (if cell :header t)))
                t))))))

(defmethod graph-element-mouse-moved ((window graph-window) (element table-element) (part (eql t)))
  (let ((class (slot-value element 'class)))
    (cg:window-message (application-window-main-window (graph-editor-window window))
      (cond ((and (graph-element-selected-p element)
                  (containerp class))
             "Use the ~~Cards menu to add a new ~~Question box")
            ((not (graph-element-selected-p element))
             "Click to select this element")
            (t
             "")))
    (cg:set-cursor cg:*screen* cg:arrow-cursor)))

(defmethod graph-element-mouse-moved ((window graph-window) (element grid-graph-element) (part cg:position))
  (if (graph-element-selected-p element)
      (progn
        (if (pro:izerop (cg:position-y part))
            (cg:window-message (application-window-main-window (graph-editor-window window))
              "Click to edit this column label")
            (cg:window-message (application-window-main-window (graph-editor-window window))
              "Click to edit this cell"))
        (cg:set-cursor cg:*screen* (get-cursor :grid)))
      (call-next-method)))

(defmethod graph-element-mouse-moved ((window graph-window) (element grid-graph-element) (part integer))
  (if (graph-element-selected-p element)
      (progn
        (cg:window-message (application-window-main-window (graph-editor-window window))
          "Drag to change the width of this column")
        (cg:set-cursor cg:*screen* (get-cursor :adjust-x)))
      (call-next-method)))

;;; Map parts to cursors; and do this a bit more efficiently. Do this by adding a table.
;;; We can use this to handle all the different cursors that we need to, handling the
;;; different element parts. 

(defconstant part-cursor-map
  '((:title :grid "Click to edit this element’s title")
    (:link :pencil "Drag to make a new link")
    (:resize :resize "Drag to change the size of this element")
    (:resize-header :resize "Drag to change the size of this element’s matching box")))

(defmethod graph-element-mouse-moved ((window graph-window) (element table-element) part)
  (let ((main-window (application-window-main-window (graph-editor-window window))))
    (if (graph-element-selected-p element)
        (let ((cursor (assoc part part-cursor-map)))
          (if cursor
              (progn
                (cg:window-message main-window (third cursor))
                (cg:set-cursor window (get-cursor (second cursor))))
              (call-next-method)))
        (progn
          (cg:window-message main-window "Click to select this element")
          (cg:set-cursor cg:*screen* cg:arrow-cursor)))))

;;; When we complete an element edit, we can generate and issue a command which will
;;; update the grid element. This command should, of course, be undoable. However, 
;;; we ought to be a bit careful because we don't want people to be able to do too
;;; much. Of course, when the value hasn't changed, there's no need to generate a 
;;; command at all. The moral is: don't generate commands with no effect. 

(defun ngrid-graph-element-part-box (element part box)
  (let ((area (graph-element-area element)))
    (cond ((cg:positionp part)
           (cg:nmake-box box (cg:position-x part) (cg:position-y part)
             (pro:i1+ (cg:position-x part)) (pro:i1+ (cg:position-y part)))
           (nmake-box-from-grid-box element box)
           (cg:nbox-move box
             (cg:nmake-position '#.(cg:make-position 0 0)
               (cg:box-left area)
               (pro:i+ (grid-graph-element-row-height element) (cg:box-top area))))
           (unless (element-template-p element)
             (pro:iincf (cg:box-top box) *template-border-offset*)
             (pro:iincf (cg:box-bottom box) *template-border-offset*)))
          ((eq part :title)
           (cg:ncopy-box box area)
           (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) (grid-graph-element-row-height element)))
           (unless (element-template-p element)
             (pro:iincf (cg:box-bottom box) *template-border-offset*))))
    box))

(defun copy-part (part)
  (cond ((cg:positionp part) (cg:copy-position part))
        (t part)))

