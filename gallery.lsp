;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; The gallery!  You can use the gallery to look through all the preprogrammed questions.  There 
;;; probably won't be that many of them to begin with, but we'll get there eventually.  

(defvar *gallery-window* ())

(defclass gallery-graph-window (graph-window)
  ()
  (:default-initargs :auto-scroll-p nil))

(defmethod canvas-event ((window gallery-graph-window) event buttons data time)
  (declare (ignore window event buttons data time)))

(defun make-gallery-window (top-level-window &aux dialog)
  (setf dialog
    (cg:open-dialog (list (cg:make-dialog-item :widget 'cg:default-button
                                               :box #.(cg:make-box 195 3 240 13)
                                               :dialog-units-p t
                                               :name :restart
                                               :title "Add to Database"
                                               :set-value-fn 'handle-debugger-restart)
                          (cg:make-dialog-item :widget 'cg:cancel-button
                                               :box #.(cg:make-box 195 15 240 25)
                                               :dialog-units-p t
                                               :name :restart
                                               :title "Close"
                                               :set-value-fn 'handle-debugger-restart)
                          
                          (cg:make-dialog-item :widget 'cg:single-item-list 
                                               :box #.(cg:make-box 2 2 60 75)
                                               :dialog-units-p t
                                               :border :black
                                               ;; :set-value-fn 'handle-debugger-select
                                               :double-click-fn #'(lambda (dialog widget)
                                                                    (handle-debugger-restart widget t nil))
                                               :name :list)
                          (cg:make-dialog-item :widget 'cg:static-text
                                               :box #.(cg:make-box 2 78 215 108)
                                               :dialog-units-p t
                                               :name :message))
      'cg:dialog top-level-window
      :font *message-font*
      :pop-up-p t
      :title "Preprogrammed Card Gallery"
      :dialog-units-p t
      :visible-box (cg:make-box 0 0 243 120)
      :window-state :normal))
  (cg:open-stream 'gallery-graph-window dialog :io
    :window-exterior '#.(cg:make-box 66 2 189 75)
    :dialog-units-p t
    :editor-window nil
    :user-scrollable nil
    :window-border :plain)
  (cg:select-window dialog)
  dialog)

(defun update-gallery-window (gallery-window)
  (let ((primitives (sort (let ((names ()))
                            (do-symbols (symbol "HANK-PRIMITIVES" names)
                              (push (get symbol 'string-name) names)))
                      #'pro:whole-string<))
        (gallery-list (cg:find-named-object :list gallery-window)))
    (setf (cg:dialog-item-range gallery-list) primitives)
    (setf (cg:dialog-item-value gallery-list) (first primitives))))

;;; Now for the icky stuff.  When a new card is selected, we need to update the graph element in 
;;; the graph window, so that people can see the parameters and all the matching box and that kind 
;;; of stuff.  This means that we need to be able to store prototypical matching boxes and things 
;;; like that, all in the gallery.  At the end, people can click "Add to Database" and get the
;;; preprogrammed box they want, added where they want it.  Copy and Paste would be better, but 
;;; that'll have to wait a little, probably. 
;;;
;;; It's actually quite important that the gallery uses the data that the primitive knows about 
;;; itself, so that we don't get lost in a world of updates. 