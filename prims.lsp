;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

(defvar *special-table-map*
  (progn
    (when (find-package "HANK-PRIMITIVES")
      (delete-package "HANK-PRIMITIVES"))
    (make-package "HANK-PRIMITIVES"
      :nicknames '("HP")
      :use ())))

(export (loop for name in '("ADD" "SAY" "COPY" "STATUS" "EMPTY" "ADD TO CARD" "REMOVE FROM CARD")
              collect (intern name "HANK-PRIMITIVES"))
  "HANK-PRIMITIVES")

(defun find-primitive-function (name)
  (let ((position (position #\: name)))
    (if position
        (find-symbol (nstring-upcase (subseq name 0 position)) *special-table-map*)
        (find-symbol (string-upcase name) *special-table-map*))))

(defmacro define-primitive (name string lambda &body body)
  `(progn
     (defun ,name (,@lambda &aux (*primitive-name* ',name))
       (declare (special *primitive-name*))
       (restart-bind ((continue #'(lambda ()
                                    (return-from ,name
                                      `((,*status-key* . "Fail")
                                        ,@(rest ,(third lambda)))))
                        :report-function
                          #'(lambda (stream)
                              (declare (special *expanded-restart-label-p*))
                                            (if *expanded-restart-label-p*
                                                (format stream "To carry on, after setting the status to \"Fail\", choose this option.")
                                                (format stream "Fail this preprogrammed question")))))
         ,@body))
     (setf (get ',name 'string-name) ',string)
     ',name))

;;; Special table handlers for a few functions. Most of these should be simple
;;; enough, particularly if we spend a little time writing the general auxiliary
;;; functions that they use to do the right kinds of parameter checks. 

(defun check-arguments (arguments count)
  (declare (special *primitive-name*))
  (unless (= (length arguments) count)
    (hank-error "The \"~A ~A\" special table expects ~A arguments" 
      *character-prefix* (get *primitive-name* 'string-name) (length arguments))))

(define-primitive hp:add "Add" (state event environment columns arguments)
  (check-arguments arguments 3)
  (destructuring-bind (input1 input2 output) arguments
    (setf input1 (evaluate-element environment input1))
    (setf input2 (evaluate-element environment input2))
    (ensure-unbound-variable environment output)
    `((,*status-key* . "OK")
      (,output . ,(write-term-to-string (+ (hank-get-number input1) (hank-get-number input2))))
      ,@(rest environment))))

(define-primitive hp:say "Say" (state event environment columns arguments)
  (let ((output-window (cg:get-stream-prop (get-control-panel-window (state-main-window state)) 'output-window)))
    (file-position output-window :end)
    (format output-window
      "~A says: ~{~A~^ ~}~%"
      *character-name*
      (loop for element in arguments
            collect (if (pro:iplusp (length element))
                        (evaluate-element environment element)
                        "")))
    `((,*status-key* . "OK")
      ,@(rest environment))))

(define-primitive hp:copy "Copy" (state event environment columns arguments)
  (check-arguments arguments 2)
  (destructuring-bind (variable value) arguments
    (ensure-unbound-variable environment variable)
    (ensure-value environment value)
    (setf value (instantiate-element environment value))
    `((,*status-key* . "OK")
      (,variable . ,value)
      ,@(rest environment))))

;;; According to the new execution model, we should never need empty.  I suppose we
;;; can add it if we need it later.  Instead, if we ever go back to a previous step,
;;; we should empty all the appropriate wildcards.  And guess what?  We've now 
;;; renamed wildcards again, they're value boxes now.  
;;;
;;; The tracer will probably need a bit of work to handle this, as will the execution
;;; model. 

(define-primitive hp:empty "Empty" (state event environment columns arguments)
  (loop for variable in arguments
        do (setf environment `((,*status-key* . "OK")
                               ,@(remove variable (rest environment)
                                   :key #'first
                                   :test #'pro:whole-string-equal))))
  environment)

(define-primitive hp:status "Status" (state event environment columns arguments)
  (check-arguments arguments 1)
  (destructuring-bind (value) arguments
    (setf value (instantiate-element environment value))
    (if (variablep value)
        `((,*status-key* . "OK")
          (,value . ,(rest (first environment)))
          ,@(rest environment))
        `((,*status-key* . ,value)
          ,@(rest environment)))))

;;; Currently, status and exit are the nearly the same, although they probably shouldn't be.  Ideally,
;;; the exit primitive should stop the interpreter no matter what. In practice, for bound values it 
;;; is the same as status. Unlike exit, status can also pick up the current status and do some 
;;; smart stuff with it. 

(define-primitive hp:exit "Exit" (state event environment columns arguments)
  (check-arguments arguments 1)
  (destructuring-bind (value) arguments
    (ensure-value environment value)
    (setf value (instantiate-element environment value))
    `((,*status-key* . ,value)
      ,@(rest environment))))

;;; Important primitives are add to database and remove from database, which cannot really be 
;;; bypassed.  These require a bit of thought, because they are specials.  
;;;
;;; These specials need to find the appropriate table, if one exists.  If none does, then they
;;; should generate a new untitled file, and add (or remove) the appropriate data from the 
;;; table in that file.  We probably need commands which preserve the kinds of ordering needed
;;; in deleting and adding to the database tables.  These commands should, of course, be 
;;; undoable, as far as the editor is concerned.  

(defun find-new-window (main-window)
  (loop for window = (cg:front-window (main-window-body main-window)) then (cg:next-window window)
        do (when (null window)
             (loop-finish))
           (when (and (typep window 'editor-window)
                      (null (editor-window-pathname window)))
             (return window))
        finally do (return-from find-new-window (values (:new main-window) t))))

(defun change-database (state event environment addp table-name table-columns table-row) ;;(state event environment database-table addp)
  (let* (;(table-name (compound-operator database-table))
         ;(table-columns (third (compound-arguments database-table)))
         ;(table-row (fourth (compound-arguments database-table)))
         
         (cards (get-interpreter-element-table (state-main-window state)))
   
         (match (assoc table-name cards :test #'pro:whole-string-equal))
         (new-environment environment))
    (when match
      (loop for card in (rest match)
            for type = (first (compound-arguments (second card)))
            do (let ((result (match table-columns (first card) environment)))
                 (unless (eq result :fail)
                   (setf new-environment result)
                   (setf match card)
                   (return nil)))
            finally do (setf match nil)))

    (cond ((and addp (null match))
           
           ;; If there's a match here, we'll have the columns, the values, and the table element in
           ;; match.  Otherwise, match will be nil, and we should create a new table which matches
           ;; all the criteria, in a new window.  This is the time, then, to create the new window. 
           ;; If we do have a match, we can find the window and the table element from the match
           ;; itself.  
           
           (multiple-value-bind (window newp) (find-new-window (state-main-window state))
             (let ((*script-command* (make-compound "New-Element"
                                       `("Fact" "Title" ,table-name 
                                                "Size" ,(list (length table-columns) 1)
                                                "Column Names" ,table-columns
                                                "Row Data" (,(loop for element in table-row
                                                                   collect (instantiate-element environment element)))))))
               (when newp (cg:shrink-window window ()))
               (setf (getf (event-properties event) 'undo-command) (:new-element window))
               t)))
          ((and (not addp) (null match)) nil)
          ((and addp match)
           
           ;; If we find a match, then there's no problem, and we can just finish.  On the other 
           ;; hand,  if we don't, then we should add a new row at the end and resize the element
           ;; accordingly.  All in one undoable command.
           
           (let* ((window (graph-element-window (third match)))
                  (command (make-command
                             :type 'add-row
                             :window window
                             :modify-window (graph-editor-window window)
                             :label "Add Row"
                             :do-function #'primitive-command-add-row-do-function
                             :undo-function #'primitive-command-add-row-undo-function
                             :do-data (list (third match)
                                            (loop for element in table-row
                                                  collect (instantiate-element environment element)))
                             :undo-data (list (third match)))))
             (setf (getf (event-properties event) 'undo-command) (execute command))
             t))
          
          ((and (not addp) match)
           
           ;; We find a match, and we're trying to remove an element (row) from a table. Do this by
           ;; find the row and marking it.  Then we can delete it.  If we need to add it again, we
           ;; can do this. Unlike the Add Row command, any row in the table can be affected, 
           ;; although we're only interested in one at a time.
           
           (loop for row in (rest (rest (rest (compound-arguments (second match)))))
                 for index from 0
                 do (let ((result (match table-row row environment)))
                      (unless (eq result :fail)
                        (let* ((element (third match))
                               (parent (graph-element-parent element))
                               (length (print (cg:position-y (table-element-body-cells element))))
                               (window (graph-element-window element))
                               (selection (graph-selected-elements element))
                               (command (if (pro:i= 1 length)
                                            (make-command :type 'remove-element
                                              :label "Remove Element"
                                              :window window
                                              :modify-window (graph-editor-window window)
                                              :do-function 'object-clipboard-command-do-function
                                              :undo-function 'object-clipboard-command-undo-function
                                              :do-data   (list (remove element selection)
                                                               (list parent () (list element)))
                                              :undo-data (list selection
                                                               (list parent (list element) ())))
                                            (make-command :type 'remove-element
                                              :label "Remove Row"
                                              :window window
                                              :modify-window (graph-editor-window window)
                                              :do-function 'primitive-command-remove-row-do-function
                                              :undo-function 'primitive-command-remove-row-undo-function
                                              :do-data   (list element index row)
                                              :undo-data (list element index row)))))
                          (setf (getf (event-properties event) 'undo-command) (execute command))
                          (return t)))))))))

(defun primitive-command-add-row-do-function (command)
  (primitive-command-add-row-apply-function command (command-do-data command) t))
(defun primitive-command-add-row-undo-function (command)
  (primitive-command-add-row-apply-function command (command-undo-data command) nil))

(defun primitive-command-add-row-apply-function (command data addp &aux length)
  (destructuring-bind (element &optional row-data) data
    (when (cg:closed-stream-p (graph-element-window element))
      (return-from primitive-command-add-row-apply-function))
    (setf length (cg:position-y (slot-value element 'body-cells)))
    (setf (slot-value element 'row-data)
          (if addp
              (concatenate 'list
                (subseq (slot-value element 'row-data) 0 length)
                (list row-data)
                (subseq (slot-value element 'row-data) length))
              (concatenate 'list
                (subseq (slot-value element 'row-data) 0 (pro:i1- length))
                (subseq (slot-value element 'row-data) length))))
    (if addp
        (pro:iincf (cg:position-y (slot-value element 'body-cells)))
        (pro:idecf (cg:position-y (slot-value element 'body-cells))))       
    (setf (graph-element-area element)
            (cg:copy-box (calculate-table-element-area element)))))
      
(defun primitive-command-remove-row-do-function (command)
  (primitive-command-remove-row-apply-function command (command-do-data command) t))
(defun primitive-command-remove-row-undo-function (command)
  (primitive-command-remove-row-apply-function command (command-undo-data command) nil))

(defun primitive-command-remove-row-apply-function (command data removep)
  (destructuring-bind (element &optional position row-data) data
    (when (cg:closed-stream-p (graph-element-window element))
      (return-from primitive-command-remove-row-apply-function))
    (setf (slot-value element 'row-data)
          (if removep
              (concatenate 'list
                (subseq (slot-value element 'row-data) 0 position)
                (subseq (slot-value element 'row-data) (pro:i1+ position)))
              (concatenate 'list
                (subseq (slot-value element 'row-data) 0 position)
                (list row-data)
                (subseq (slot-value element 'row-data) position))))
    (if removep
        (pro:idecf (cg:position-y (slot-value element 'body-cells)))
        (pro:iincf (cg:position-y (slot-value element 'body-cells))))       
    (setf (graph-element-area element)
            (cg:copy-box (calculate-table-element-area element)))))
      
(define-primitive hp:|ADD TO CARD| "Add to Card" (state event environment columns arguments)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask)))
         (table-name (question-table current-question))
         (table-columns columns)
         (table-row arguments)
         (position (1+ (position #\: table-name :start (1+ (position #\: table-name))))))
    (setf table-name (canonicalise-value (subseq table-name position)))
    `((,*status-key* . ,(if (change-database state event environment t table-name table-columns table-row)
                            "OK"
                            "Fail"))
      ,@(rest environment))))

(define-primitive hp:|REMOVE FROM CARD| "Remove from Card" (state event environment columns arguments)
  (let* ((current-ask (first (state-current-asks state)))
         (current-question (or (first (ask-steps current-ask)) (ask-question current-ask)))
         (table-name (question-table current-question))
         (table-columns columns)
         (table-row arguments)
         (position (1+ (position #\: table-name :start (1+ (position #\: table-name))))))
    (setf table-name (canonicalise-value (subseq table-name position)))
    `((,*status-key* . ,(if (change-database state event environment nil table-name table-columns table-row)
                            "OK"
                            "Fail"))
      ,@(rest environment))))

