;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

;;; More top level stuff, which will generally come almost at the end of the load process.
;;; This will contain the really high level stuff. We hope that it's all OK. 
;;;
;;; More selection handling stuff. These should set up the elements menu to reflect the
;;; current selection. 
;;;
;;; The modified menu for creating cards and boxes needs a slightly different system
;;; for deciding what to create and which items need to be enabled. The policy is that
;;; all the card creation items will generally be permanently enabled. The box creation
;;; items will be enabled if the container is unambiguous. For containers, boxes are
;;; created inside, for non containers, they are created outside. 

(defun selection-type-create-allowed-p (selection type)
  t)

;;;   (case (element-type-type (assoc type *element-types*))
;;;     ((:card) t)
;;;     ((:box)
;;;      (let ((containers ()))
;;;        (loop for element in selection
;;;              do (cond ((typep element 'link-element)
;;;                        (setf element (contact-element (connection-to (link-element-connection element))))))
;;;                 (pushnew (if (element-type-container-p 
;;;                                (assoc (table-element-class element) *element-types*))
;;;                              element
;;;                              (graph-element-parent element))
;;;                   containers))
;;;        (when (and (= (length containers) 1)
;;;                   (typep (first containers) 'table-element))
;;;          t)))))

(defun selection-match-class-p (selection type)
  (every #'(lambda (element)
             (and (typep element 'table-element)
                  (eq type (element-type-type (assoc (table-element-class element) *element-types*)))))
         (the list selection)))

(defmethod selection-show-menu ((name (eql :cards)) window menu selection)
  (loop for type in *element-types*
        for menu-item = (cg:find-named-object (element-type-class type) menu)
        if menu-item
          do (cg:set-menu-item-available-p menu-item 
               (selection-type-create-allowed-p selection (element-type-class type))))
  (let* ((cards-menu-item (cg:find-named-object :change-type-cards menu))
         (boxes-menu-item (cg:find-named-object :change-type-boxes menu))
         (cards (when cards-menu-item (cg:menu-items (cg:menu-item-value cards-menu-item))))
         (boxes (when boxes-menu-item (cg:menu-items (cg:menu-item-value boxes-menu-item))))
         (labels ())
         (classes ())
         (types ()))
    (loop for element in selection
          if (typep element 'link-element)
            do (pushnew :link types)
               (unless (find (link-element-label element) labels :test #'pro:whole-string-equal)
                 (push (link-element-label element) labels))
          else
            do (pushnew (table-element-class element) classes)
               (pushnew (element-type-type (assoc (table-element-class element) *element-types*)) types))
    (when cards-menu-item
      (cg:set-menu-item-available-p cards-menu-item (and (eq (first types) :card) (null (rest types)))))
    (when boxes-menu-item
      (cg:set-menu-item-available-p boxes-menu-item (and (eq (first types) :box) (null (rest types)))))
    (loop for item in cards
          do (cg:set-menu-item-selected-p item
               (and (eq (first classes) (cg:menu-item-name item)) (null (rest classes)))))
    (loop for item in boxes
          do (cg:set-menu-item-selected-p item
               (and (eq (first classes) (cg:menu-item-name item)) (null (rest classes)))))
    (let ((label-link-ok (cg:find-named-object :label-link-ok menu))
          (label-link-fail (cg:find-named-object :label-link-fail menu))
          (unlabel-link (cg:find-named-object :unlabel-link menu))
          (only-links-selected-p (and (eq (first types) :link) (null (rest types)))))
      (cg:set-menu-item-available-p label-link-ok only-links-selected-p)
      (cg:set-menu-item-available-p label-link-fail only-links-selected-p)
      (cg:set-menu-item-available-p unlabel-link only-links-selected-p)
      (cg:set-menu-item-selected-p label-link-ok
        (and (first labels) (pro:whole-string-equal (first labels) "OK") (null (rest labels))))
      (cg:set-menu-item-selected-p label-link-fail
        (and (first labels) (pro:whole-string-equal (first labels) "Fail") (null (rest labels))))
      (cg:set-menu-item-selected-p unlabel-link
        (and (first labels) (pro:izerop (length (first labels))) (null (rest labels)))))))

;;; Having got this far, we can now look at implementing some of the commands that 
;;; we've got. These should mostly be fairly easy, because all the commands already
;;; exist in one sense or another. The difference is in the way they were implemented
;;; before, and how we get them to connect to the new system. 

(comtab:defmenu-command :label-link-ok ((window editor-window))
  (selection-label-link window "OK"))
(comtab:defmenu-command :label-link-fail ((window editor-window))
  (selection-label-link window "Fail"))
(comtab:defmenu-command :unlabel-link ((window editor-window))
  (selection-label-link window ""))

(comtab:defmenu-command :label-link ((window editor-window))
  (let ((command *script-command*))
    (when command
      (let ((value (first (compound-arguments command))))
        (when (stringp value)
          (selection-label-link window value))))))

(defun selection-label-link (editor-window label)
  (multiple-value-bind (window elements) (:selection editor-window t)
    (complete-element-editor window :dont-save)
    (setf elements (remove-if #'(lambda (element)
                                  (pro:whole-string-equal (link-element-label element) label))
                              elements))
    (unless elements (return-from selection-label-link nil))
    (let ((command (make-command
                     :type 'change-label
                     :window window
                     :modify-window (graph-editor-window window)
                     :label "Label"
                     :do-data (loop for element in elements
                                    collect (list element label))
                     :undo-data (loop for element in elements
                                      collect (list element (link-element-label element)))
                     :do-function #'link-label-command-do-function
                     :undo-function #'link-label-command-undo-function)))
      (execute command)
      (record-command (application-window-main-window editor-window)
        (make-compound "Label Link" (list label))))))

;;; Creating new elements. This needs to be handled with a bit of care. However,
;;; all should be OK somehow. We also need to be able to change the type of an
;;; existing element, and all the information associated with it should not be
;;; lost for the saved file. Of course, it doesn't really matter how much space
;;; we need when we have to deal with this. 

(defgeneric :new-element (window &key type))
(defmethod :new-element ((window t) &key type) nil)
(defmethod :new-element ((window pro:simple-stream) &key type)
  (:new-element (cg:selected-window window) :type type))
(setf (get :new-element 'play-command-arguments) (list :type nil))

;;; The first thing we need is an element which tells us the parent for the
;;; new element. This should be unambiguous, so we should be able to determine
;;; it from the selection quickly. 
;;;
;;; When scripting, here, we should remember to keep the command type, so that we
;;; can reconstruct this element. Actually, it would also help if we could get the
;;; position, and convert that to points rather than pixels, but for now, a lot of
;;; this can wait. 

(defmethod :new-element ((window graph-window) &key type)
  (let* ((command *script-command*)
         (arguments (when command (compound-arguments command)))
         (class (or (when command
                      (let ((type (find (first arguments) *element-types* :test #'string-equal :key #'element-type-label)))
                        (when type (element-type-class type))))
                    type
                    (error "No type for new element")))
         (selection (graph-selected-elements window))
         (parent (cond ((null selection) window)
                       ((typep (first selection) 'link-element)
                        (graph-element-parent (first selection)))
                       ((eq :card (element-type-type (assoc class *element-types*)))
                        window)
                       ((element-type-container-p (assoc (table-element-class (first selection)) *element-types*))
                        (first selection))
                       (t
                        (graph-element-parent (first selection)))))
         (size (when command (string-getf (rest arguments) "Size")))
         (title (or (when command (string-getf (rest arguments) "Title")) "Untitled"))
         (row-data (when command (string-getf (rest arguments) "Row Data")))
         (columns (or (first size) *default-table-columns*))
         (rows (or (second size) *default-table-rows*))
         (column-names (when command (string-getf (rest arguments) "Column Names"))))
    (let* ((graph-element
             (make-instance 'table-element
               :title title
               :window window
               :body-cells (cg:make-position columns rows)
               :column-widths (make-list columns :initial-element *default-column-width*)
               :template ;; (if (or (eq class :command_card) (eq class :special_box))
                         ;;     ()
                             (or column-names (make-list columns :initial-element ""))
                         ;;     )
               :row-data row-data
               :class class
               :parent parent))
           (command (make-command
                      :type 'new-element
                      :window window
                      :modify-window (graph-editor-window window)
                      :do-data (list (list graph-element) (list parent (list graph-element) ()))
                      :undo-data (list (graph-selected-elements window) (list parent () (list graph-element)))
                      :label "New Element"
                      :do-function #'object-clipboard-command-do-function
                      :undo-function #'object-clipboard-command-undo-function))
           (position '#.(cg:make-position 0 0)))
     (nset-new-element-position parent position)
     (setf (slot-value graph-element 'area) (cg:make-box-from-corners position position))
     (setf (graph-element-area graph-element t)
             (cg:copy-box (calculate-table-element-area graph-element)))
     (record-command (application-window-main-window (graph-editor-window window))
       (make-compound "New Element"
         `(,(element-type-label (assoc type *element-types*))
           "Position" ,(list (unresolve (cg:position-x position)) (unresolve (cg:position-y position)))
           "Size" ,(let ((size (slot-value graph-element 'body-cells)))
                     (list (cg:position-x size) (cg:position-y size))))))
     (execute command)
     command)))

