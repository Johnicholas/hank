;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University
;;;
;;; More spreadsheet interface stuff. The stuff here provides the spreadsheet
;;; editing system. This is implemented with a whole bunch of useful stuff. 
;;; The idea is that we can do smart things here. 

(in-package "HANK")

;;; After the end of a select loop, we can do some additional stuff
;;; with the part where the mouse went down. This is needed so that
;;; we can open a small text editor for an item's value when we need
;;; to. We should only do this if we don't drag! And also, if we 
;;; go on to double-click, we should be a bit smarter about handling
;;; that too. We should also close the element editor at various times
;;; in the system, like when we're about to scroll or, especially, 
;;; drag. 
;;; 
;;; As soon as the element editor has changed something, we want to 
;;; make up an undo command that will handle it. That is, we don't want
;;; people to have to complete the edit before they can undo it. We can
;;; handle this (I think) as a special undo command for the little text
;;; editor, or we can handle it as an accumulating undo commmand which
;;; changes as the text changes. But we do need to handle it one way or
;;; another. 
   
(defmethod selected-element ((element table-element) part window)
  (complete-element-editor window :new-table))

(defmethod selected-element ((element table-element) (part (eql :enter)) window)
  (let ((class (table-element-class element)))
    (if (and (eq class :question_box) (typep (graph-element-parent element) 'cg:window))
        (if (pro:ilogtest cg:alt-key (cg:mouse-button-state))
            (:ask-in-full (frame-window-of-window window))
            (:ask-briefly (frame-window-of-window window)))
        (selected-element element :title window))))

(defun ask-install-question (window)
  (let ((graph-window (cg:selected-window window)))
    (complete-element-editor graph-window :end)
    (let ((selection (graph-selected-elements graph-window)))
      (when (and (= (length selection) 1)
                 (typep (first selection) 'table-element)
                 (eq :question_box (table-element-class (first selection))))
        (complete-ask-question window (first selection))))))

;;; Handle with a bit of care, for scripting reasons. We need the script system
;;; to not necessarily do too much here. This is because one command calls another
;;; here. In fact, about three things happen. (1) copy the table to the control
;;; panel, (2) select the control panel window, and (3) do the ask itself. Make
;;; all these different commands is best. 

(defmethod selected-element ((element table-element) (part cg:position) window)
  (complete-element-editor window :new-table)
  (ensure-grid-graphics-context window element part)
  (let ((area (grid-graph-element-cell-box element part)))
    (setf (cg:box-bottom area) (pro:i+ (cg:box-top area) (pro:i- (grid-graph-element-row-height element) 2)))
    (pro:iincf (cg:box-left area) (pro:i1- *spreadsheet-border-x*))
    (pro:iincf (cg:box-top area) *spreadsheet-border-y*)
    (install-element-editor element window area ()
      (grid-graph-element-cell-data element part)
      `(element ,element part ,(copy-part part) function complete-element-cell-editor-function))))

;;; The other element that can be selected is the title. This is rather similar, except
;;; that because we're not editing a cell, we need to be a bit more careful with the
;;; data. 

(defmethod selected-element ((element table-element) (part (eql :title)) window)
  (complete-element-editor window :new-table)
  (ensure-grid-graphics-context window element part)
  (let ((area (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-area element))))
    (setf (cg:box-bottom area) (pro:i+ (cg:box-top area) (grid-graph-element-row-height element)))
    (setf (cg:box-bottom area) (pro:i+ (cg:box-top area) (pro:i- (grid-graph-element-row-height element) 2)))
    (pro:iincf (cg:box-left area) (pro:i1- *spreadsheet-border-x*))
    (pro:iincf (cg:box-top area) *spreadsheet-border-y*)
    (install-element-editor element window area () (table-element-title element)
      `(element ,element part :title function complete-element-title-editor-function))))

(defmethod selected-element ((element null) part window)
  (complete-element-editor window :new-table)
  (cg:set-cursor window cg:arrow-cursor))
   
;;; When we install the element editor, add it to the window and store the completion
;;; function there too. The completion function will be called from time to time. 
;;; We use this to do things when the text has been completed. 

(defmethod cg:event ((window element-pane) (event (eql cg:virtual-key-down)) buttons (data (eql machine:vk-enter)) time)
  (let* ((window (cg:stream-location window))
         (editor-descriptor (getf (cg:stream-plist window) 'element-editor)))
    (complete-element-editor window :end)))

(defmethod cg:event ((window element-pane) (event (eql cg:virtual-key-down)) buttons (data (eql machine:vk-return)) time)
  (cg:event window event buttons machine:vk-enter time))

(defmethod cg:event ((window element-pane) (event (eql cg:virtual-key-down)) buttons (data (eql machine:vk-tab)) time)
  (let* ((window (cg:stream-location window))
         (editor-descriptor (getf (cg:stream-plist window) 'element-editor)))
    (unless editor-descriptor
      (return-from cg:event))
    (let* ((part (getf (rest editor-descriptor) 'part))
           (element (getf (rest editor-descriptor) 'element))
           (cells (grid-graph-element-cells element)))
      (cond ((cg:positionp part)
             (let ((part (cg:ncopy-position '#.(cg:make-position 0 0) part)))
               (pro:iincf (cg:position-x part))
               (when (pro:i= (cg:position-x part) (cg:position-x cells))
                 (setf (cg:position-x part) 0)
                 (pro:iincf (cg:position-y part)))
               (when (pro:i= (cg:position-y part) (cg:position-y cells))
                 (setf part :title))
               (selected-element element part window)
               ))
            ((eq part :title)
             (setf part (cg:nmake-position '#.(cg:make-position 0 0) 0 0))
             (selected-element element part window)
             )))))

(defun complete-element-editor (window status)
  (setf (getf (cg:stream-plist window) 'pending-selection) nil)
  (let ((editor-descriptor (getf (cg:stream-plist window) 'element-editor)))
    (unless editor-descriptor
      (return-from complete-element-editor))
    (let ((editor (first editor-descriptor))
          (function (getf (rest editor-descriptor) 'function)))
      (when (and function (not (eq status :dont-save))) (funcall function editor editor-descriptor status))
      (close editor)
      (setf (getf (cg:stream-plist window) 'element-editor) nil))
    (cond ((eq status :end) (cg:set-cursor window cg:arrow-cursor)))))

(defmethod :selection ((window element-pane) &optional no-text-p)
  (declare (ignore no-text-p))
  (multiple-value-bind (start end) (ew-highlight-range (machine:window-handle window))
    (get-window-text window)
    (let ((string (subseq (teb) start end)))
      (values window (unless (pro:izerop (length string))
                       string)))))

;;; Now we can define some command stuff which will help us to change part of a 
;;; grid. These commands will be generated when complete-element-editor decides 
;;; that a part value has changed. 

(defun complete-element-title-editor-function (window editor-descriptor status)
  (let* ((element (getf (rest editor-descriptor) 'element))
         (graph-window (graph-element-window element)))
    (set-selection graph-window (list element) :deselectp t)
    (:rename graph-window (canonicalise-value (subseq (teb) 0 (get-window-text window))))))

(defun complete-element-cell-editor-function (window editor-descriptor status)
  (let* ((element (getf (rest editor-descriptor) 'element))
         (graph-window (graph-element-window element)))
    (set-selection graph-window (list element) :deselectp t)
    (:edit graph-window 
      (getf (rest editor-descriptor) 'part) 
      (canonicalise-value (subseq (teb) 0 (get-window-text window))))))

(defgeneric (setf grid-graph-element-cell-data) (value element cell &optional recursivep))

(defun element-edit-cell-command-apply-function (command data)
  (destructuring-bind (element part value) data
    (setf (grid-graph-element-cell-data element part) value)))
(defun element-edit-cell-command-do-function (command)
  (element-edit-cell-command-apply-function command (command-do-data command)))
(defun element-edit-cell-command-undo-function (command)
  (element-edit-cell-command-apply-function command (command-undo-data command)))

(defgeneric (setf table-element-title) (value table-element &optional recursivep))

(defun element-edit-title-command-apply-function (command data)
  (destructuring-bind (element part value) data
    (setf (table-element-title element) value)))
(defun element-edit-title-command-do-function (command)
  (element-edit-title-command-apply-function command (command-do-data command)))
(defun element-edit-title-command-undo-function (command)
  (element-edit-title-command-apply-function command (command-undo-data command)))

