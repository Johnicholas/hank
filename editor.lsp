;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; Revise the editor window class. We now want to add a palette to the left side of
;;; the window. This palette can contain tools and other buttons. For this reason,
;;; we can't use cg:frame-with-single-child any more. On the whole this is not 
;;; going to be a problem, because we've actually been quite good about using it
;;; correctly. 

(defclass editor-window (application-window)
  ((pathname
     :initform nil
     :initarg :pathname
     :reader editor-window-pathname)
   (editor
     :reader editor-window-editor)
   (modifiedp
     :initform nil
     :reader :modified-p))
  (:default-initargs :user-scrollable nil))

(defmethod cg:select-window ((window editor-window) &optional recursivep)
  (let ((front-window (cg:selected-window (cg:stream-location window))))
    (unless (eq front-window window)
      (when front-window
        (notify-window-selection front-window nil))
      (when *record-select-window-p*
        (let ((main-window (application-window-main-window window)))
          (record-command main-window
            (make-compound "Select Document"
              (generate-objects-resolver (list window) main-window)))))
      (notify-window-selection window t)))
  (call-next-method))

(defmethod object-container ((window editor-window))
  (application-window-main-window window))
(defmethod object-name ((window editor-window))
  (cg:stream-title window))
(defmethod object-type ((window editor-window))
  "Document")

;;; When an editor window is opened, if its pathname is set, so is its stream title.
;;; And at any later stage, if the pathname is changed, so is the stream title. If
;;; there's no pathname, we use the *stream-title-index* to generate a stream title.

(defparameter *stream-title-index* 0)

(defmethod cg:device-open ((window editor-window) options)
  (prog1 (call-next-method)
    (let ((class (or (getf options :pane-class)
                     (cg:default-pane-class window))))
      (assert class () "Missing pane class for an editor window")
      (let ((interior (cg:nvisible-box window '#.(cg:make-box 0 0 0 0))))
        (setf (slot-value window 'editor)
                (cg:open-stream class window :io
                  :window-border :none
                  :user-scrollable t
                  :editor-window window
                  :right-attachment :right
                  :bottom-attachment :bottom
                  :window-exterior interior))))
    (setf (cg:stream-title window)
            (let ((pathname (editor-window-pathname window)))
              (if pathname
                  (file-namestring pathname)
                  (format () "Untitled~[~;~:; ~:*~D~]" (incf *stream-title-index*)))))))

(defmethod (setf editor-window-pathname) (value (window editor-window))
  (setf (cg:stream-title window) (file-namestring value))
  (setf (slot-value window 'pathname) value))

(defmethod container-objects ((window editor-window))
  (sorted-file-elements (graph-elements (slot-value window 'editor))))

;;; Note that the palette should really be on a toolbar, or another
;;; floating window. 

(defun get-editor-file-elements (window)
  (let ((elements (cg:get-stream-prop window 'file-elements :none)))
    (if (eq elements :none)
        (cg:set-stream-prop window 'file-elements
          (get-file-elements (graph-elements (editor-window-editor window))))
        elements)))

(comtab:defmenu-command (setf :modified-p) (value (window editor-window))
  (unless (eq value (slot-value window 'modifiedp))
    (setf (slot-value window 'modifiedp) value)
    (te::set-modification-flagged-p window value)
    (te::update-window-title window))
  (cg:set-stream-prop window 'file-elements :none)
  (cg:set-stream-prop window 'interpreter-elements :none)
  (cg:set-stream-prop (application-window-main-window window) 'interpreter-element-table :none)
  value)

;;; Whenever a command changes the modification state of a window, we 
;;; should call this command. Note that this should operate successfully
;;; for commands and everything. 

(defmethod (setf :modified-p) (value window)
  value)

(defmethod (setf :modified-p) (value (window cg:simple-stream))
  (let ((child (cg:selected-window window)))
    (when child
      (setf (:modified-p child) value)))
  value)

(defmethod cg:user-close ((window editor-window))
  (record-command (application-window-main-window window) "Close")
  (let ((modifiedp (:modified-p window)))
    (when modifiedp
      (ecase (pop-up-message-dialog cg:*screen* "Save Changes"
               (format () "Save changes to ~A?" (cg:stream-title window)) cg:warning-icon
               :yes :discard :cancel)
        (1 (:save window))
        (2 nil)
        (3 (return-from cg:user-close nil))))
    (let ((main-window (application-window-main-window window)))
      (cg:set-stream-prop main-window 'interpreter-element-table :none)
      (notify-window-selection window nil)
      (let ((parent (cg:stream-location window)))
        (remove-window window main-window)
        (close window)
        (notify-window-selection (cg:selected-window parent) t))
      t)))

