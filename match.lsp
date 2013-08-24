;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; Noooooo, not another unifier. Well, actually all we need is a simple matcher, so 
;;; that's all there is. This is enough for us to be able to build the basic system
;;; following the matching rules in the assignment. 

(defun match1 (pattern-element data-element environment)
  (cond ((stringp pattern-element)
         (if (stringp data-element)
             (if (variablep pattern-element)
                 (if (pro:i<= (length pattern-element) 1)
                     environment
                     (let ((match (assoc pattern-element environment :test #'pro:whole-string-equal)))
                       (if (null match)
                           (if (variablep data-element)
                               environment
                               (list* (first environment)
                                      (cons pattern-element data-element)
                                      (rest environment)))
                           (if (pro:whole-string-equal (rest match) data-element)
                               environment
                               :fail))))
                 (cond ((variablep data-element) environment)
                       ((pro:whole-string-equal pattern-element data-element) environment)
                       (t :fail)))
              :fail))
        ((null pattern-element)
         (if (null data-element)
             environment
             :fail))
        ((and (listp pattern-element) (listp data-element))
         (if (= (length pattern-element) (length data-element))
             (loop with new-environment = environment
                   for p in pattern-element
                   for d in data-element
                   do (setf new-environment (match1 p d new-environment))
                   if (eq new-environment :fail)
                     do (return :fail)
                   end
                   finally do (return new-environment))
             :fail))
        (t
         :fail)))

(defun match (pattern-element data-element environment)
  (match1 pattern-element data-element environment))

;;; There's a bit more cleaning that can help. Basically, if we clean the forms before
;;; we use them, from the file representation to something far more canonical, ensuring
;;; for example that every cell is represented and none outside the box are present. If
;;; we do this, most of the mappings are simple. 

(defmethod get-interpreter-elements ((window editor-window))
  (let ((elements (cg:get-stream-prop window 'interpreter-elements :none)))
    (if (eq elements :none)
        (cg:set-stream-prop window 'interpreter-elements
          (loop for (element graph-element) in (get-editor-file-elements window)
                for new-element = (convert-file-element-to-interpreter-element element)
                if new-element
                  collect (list new-element graph-element)))
        elements)))

(defmethod get-interpreter-elements ((window t))
  nil)

(defun get-interpreter-element-table (window)
  (let ((table (cg:get-stream-prop window 'interpreter-element-table :none)))
    (if (eq table :none)
        (loop with table = nil
              for application-window in (cg:windows (main-window-body window))
              for elements = (get-interpreter-elements application-window)
              do (loop for (element graph-element) in elements
                       for operator = (compound-operator element)
                       for arguments = (compound-arguments element)
                       for columns = (third arguments)
                       for cards = (assoc operator table :test #'pro:whole-string-equal)
                       unless (pro:whole-string-equal (first arguments) "Question_Box")
                       if cards
                         do (when (rest (assoc columns (rest cards) :test #'equalp))
                              (setf columns
                                (loop for column in columns
                                      collect (if (zerop (length column)) "<empty>" column)))
                              (error 'hank-error
                                :format-control "Card ~A with ~{~A~^, ~}already defined"
                                :format-arguments (list operator columns)))
                            (push (list columns element graph-element) (rest cards))
                       else
                         do (push (cons operator (list (list columns element graph-element))) table)
                       end
                       end)
              finally do (return (cg:set-stream-prop window 'interpreter-element-table table)))
        table)))

(defun normalise-element-row (row length)
  (let ((row-length (length row)))
    (cond ((= row-length length) row)
          ((< row-length length) (append row (make-list (- length row-length) :initial-element "")))
          ((> row-length length) (subseq row 0 length)))))

(defun normalise-element-rows (rows length)
  (loop for row in rows
        collect (normalise-element-row row length)))

;;; We could do with some preflight testing here.  However, this requires us to have some error
;;; handling system, and stuff like that!  We had a lot of this in Ousel, but we need to bring
;;; it in here and yet keep it simple.  

(defun convert-file-element-to-interpreter-element (element)
  (let* ((operator (compound-operator element))
         (arguments (compound-arguments element))
         (type (first arguments))
         (length (first (third arguments)))
         (row-data (rest (rest (rest (rest arguments)))))
         (row-data-length (length row-data))
         (row-count nil))
    (cond ((cond ((pro:whole-string-equal type "Fact_Card") (setf row-count (second (third arguments))) t)
                 ((pro:whole-string-equal type "Question_Box") (setf row-count 1) t)
                 (t nil))
           (make-compound operator
             (list* (first arguments) nil 
               (normalise-element-row (fourth arguments) length)
               (normalise-element-rows (cond ((pro:i> row-data-length row-count)
                                              (subseq row-data 0 row-count))
                                             ((pro:i< row-data-length row-count)
                                              (append row-data (make-list (pro:i- row-count row-data-length))))
                                             (t
                                              row-data))
                 (first (third arguments))))))
          ((pro:whole-string-equal type "Instruction_Card")
           (make-compound operator
             (list* (first arguments) nil 
               (normalise-element-row (fourth arguments) length)
               (normalise-element-row (fifth arguments) length)
               (loop for element in (nthcdr 5 arguments)
                     collect (cond ((compound-p element)
                                    (first (convert-file-elements-to-interpreter-elements (list element))))
                                   ((null element) nil)
                                   ((listp element)
                                    (cond ((pro:whole-string-equal (first element) "If")
                                           (subseq element 0 3))
                                          (t
                                           element)))
                                   (t
                                    element))))))
          #||
          ((pro:whole-string-equal type "Special_Box")
           (print (make-compound operator
             (list* (first arguments) nil 
               (normalise-element-row (fourth arguments) length)
               (normalise-element-row (fifth arguments) length)
               (loop for element in (nthcdr 5 arguments)
                     collect (cond ((compound-p element)
                                    (first (convert-file-elements-to-interpreter-elements (list element))))
                                   ((null element) nil)
                                   ((listp element)
                                    (cond ((pro:whole-string-equal (first element) "If")
                                           (subseq element 0 3))
                                          (t
                                           element)))
                                   (t
                                    element)))))))
          ||#
          (t
           (error "Can't run programs containing elements of type ~A" type)))))

(defun convert-file-elements-to-interpreter-elements (elements)
  (loop for element in elements
        for new-element = (convert-file-element-to-interpreter-element element)
        if new-element
          collect new-element))

;;; We can also use this when we're using the control panel. This, too, generates a file
;;; element that we can convert into an interpreter element. 
;;;
;;; Special cards are handled -- specially. Many can be mapped semi-automatically from
;;; Lisp. Note, though, that there are data type implications, because Hank effectively
;;; deals only with string data types. The interpreter handles these by matching looking
;;; for input and output values. Only when it finds an appropriate match will it 
;;; use it. We handle most things by mapping strings to functions with a few extra 
;;; properties that we can use to look for types and stuff like that.
;;;
;;; All special boxes are called as functions with two args, an environment and a set
;;; of values. They all return a new environment. The status is coded in that environment
;;; as a special marker. If there's a problem, we should signal it by a few special error
;;; classes. These can then do something a bit more interesting in the interface, but we
;;; can work all that out later. 

(defun hank-error (control &rest arguments)
  (error 'hank-error
    :format-control control
    :format-arguments arguments))

(defclass hank-error (simple-error)
  ())

(defclass hank-type-error (hank-error)
  ())

(defun hank-get-number (string)
  (multiple-value-bind (class value index) (read-value-from-string string)
    (if (and (or (eq class 'float)
                 (eq class 'integer))
             (or (= index (length string))
                 (not (find #\Space string :start index :test-not #'eql))))
        value
        (error 'hank-type-error 
          :format-control "Invalid number: \"~A\"" 
          :format-arguments (list string)))))

(defun evaluate-element (environment variable)
  (if (variablep variable)
      (or (rest (assoc variable environment :test #'pro:whole-string-equal))
          (error-unbound-variable variable))
      variable))

(defun instantiate-element (environment element)
  (if (variablep element)
      (or (rest (assoc element environment :test #'pro:whole-string-equal))
          element)
      element))

(defun instantiate-list (environment list)
  (loop for element in list
        collect (instantiate-element environment element)))

(defun ensure-unbound-variable (environment variable)
  (cond ((not (variablep variable))
         (hank-error "\"~A\" is not a value box" variable))
        ((assoc variable environment :test #'pro:whole-string-equal)
         (hank-error "Value box \"~A\" already has an acquired value" variable))))

(defun error-unbound-variable (variable)
  (hank-error "Wildcard \"~A\" has no acquired value" variable))

(defun ensure-value (environment value)
  (when (and (variablep value)
             (not (assoc value environment :test #'pro:whole-string-equal)))
    (hank-error "Value box \"~A\" does not have an acquired value" value)))

