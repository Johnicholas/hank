;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; We've got as far as the interpreter, at last. This will borrow significant
;;; elements from the new Froglet evaluator, but should be a bit simpler. The
;;; idea, however, will be basically the same. We use a state and event system,
;;; which like the command system, is generally undoable. This means the interpreter
;;; is, in principle at least, reversible. In practice, you'll need access to the
;;; right commands and to a stored set of events to run a query backwards, and that
;;; may not always be available. However, the idea is generally pretty simple. 
;;;
;;; I suppose the best thing to do is simply to get on with writing the interpreter. A
;;; lot of this is defined in the Hank TMA 03 text, so I won't expand on it in too much
;;; detail. Mostly, this simply follows the standard instruction sheet. We also need
;;; to handle the basic matching rules, but that's far less serious. 

(defun %%execute (state restart
                  &aux current-ask current-table current-card current-question cards
                       value new-environment)
  (tagbody
    restart
    (cond ((eq restart :find-card) (go restart-find-card))
          ((eq restart :ask) (go restart-ask))
          ((eq restart :start) (go restart-find-card))
          ((eq restart :next-question) (go restart-next-question))
          ((eq restart :complete) (go restart-complete))
          ((eq restart :continue) (go restart-continue))
          ((eq restart :ask-user-card) (go restart-ask-user-card))
          ((eq restart :continue-instruction-card) (go restart-continue-instruction-card)))
    (error "Invalid restart")
    
    restart-find-card
    (setf current-ask (first (state-current-asks state)))
    (setf current-question (or (first (ask-steps current-ask)) (ask-question current-ask)))
    
    (when (question-card current-question)
      (return-from %%execute
        (make-event
          :class :found-card
          :do-start :ask
          :undo-start restart)))
    
    (setf current-table (question-table current-question))
    (unless (and (>= (length current-table) 5)
                 (string-equal current-table *character-prefix* :end1 (length *character-prefix*)))
      (go restart-find-user-card))
    
    (let* ((name (canonicalise-value (subseq current-table (length *character-prefix*))))
           (match (find-primitive-function (string-upcase name))))
      (unless match
        (error 'hank-error
          :format-control "~A doesn't know how to \"~A\""
          :format-arguments (list *character-name* name)))
      
      ;; At this point, we know that we can deal with a match. We can get the function
      ;; and generate an event which takes us to the next step. Note, however, that the
      ;; continuation needs to be handled with a little more care than we have so far. 
       
      (return-from %%execute
        (make-event
          :class :answer
          :subclass :special
          :do-start :ask
          :do-data (list match)
          :do-function 'event-do-special-card
          :undo-start restart
          :undo-function 'event-undo-special-card)))
    
    restart-find-user-card
    (setf cards (get-interpreter-element-table (state-main-window state)))
    (let ((match (assoc current-table cards :test #'pro:whole-string-equal))
          (environment (ask-environment current-ask))
          (columns (question-column current-question))
          (seek (question-seek current-question)))
      (unless match (go restart-answer-no))
      (loop with new-environment
            for card in (rest match)
            for type = (first (compound-arguments (second card)))
            do (setf new-environment (match columns (first card) environment))
            if (not (eq new-environment :fail))
              do (return-from %%execute
                   (make-event
                     :class :found-card
                     :do-start :ask
                     :do-function 'event-do-user-card
                     :do-data (list (second card))
                     :undo-start restart
                     :undo-function 'event-undo-user-card))
            end))
    (go restart-answer-no)
    
    restart-ask
    (setf current-ask (first (state-current-asks state)))
    (setf current-question (or (first (ask-steps current-ask)) (ask-question current-ask)))
    (setf current-table (question-table current-question))
    
    restart-ask-special-card
    (setf current-card (question-card current-question))
    (when (compound-p current-card) (go restart-ask-fact-card))
    
    (return-from %%execute
      (make-event
        :class :answer
        :subclass :special
        :do-start :next-question
        :do-data (list current-card (question-column current-question) (question-seek current-question))
        :do-function 'event-do-special
        :undo-start restart
        :undo-function 'event-undo-special))
    
    restart-ask-user-card
    (setf current-ask (first (state-current-asks state)))
    (setf current-question (or (first (ask-steps current-ask)) (ask-question current-ask)))
    (setf current-table (question-table current-question))
    
    restart-ask-fact-card
    (setf current-card (question-card current-question))
    (setf value (compound-arguments current-card))
    (unless (pro:whole-string-equal (first value) "Fact_Card")
      (go restart-ask-instruction-card))
    
    ;; Value contains the card body. 1st = type, 2nd = plist, 3rd = columns, then body
    
    (setf new-environment (match (question-column current-question) (third value) (ask-environment current-ask)))
    (assert (not (eq new-environment :fail)))
    (loop with seek = (question-seek current-question)
          for entry in (rest (rest (rest value)))
          for more-environment = (match seek entry new-environment)
          if (not (eq more-environment :fail))
            do (return-from %%execute
                 (make-event
                   :class :answer
                   :subclass :fact
                   :do-data (list entry more-environment)
                   :do-function 'event-do-fact
                   :do-start :next-question
                   :undo-function 'event-undo-fact
                   :undo-start restart))
          end)
    (go restart-answer-no)
    
    restart-ask-instruction-card
    (unless (pro:whole-string-equal (first value) "Instruction_Card")
      (go restart-answer-no))
    
    ;; When processing an instruction card, we need to be a bit more careful. Column
    ;; and value bindings should both be available in the instruction card body. 
    
    (when (ask-steps current-ask) (go restart-ask-new))
    
    (setf new-environment (list (cons *status-key* "OK")))
    (setf new-environment (match (third value) (question-column current-question) new-environment))
    (assert (not (eq new-environment :fail)))
    
    (setf new-environment (match (fourth value) (question-seek current-question) new-environment))
    
    (return-from %%execute
      (make-event
        :class :instruction
        :do-data (list current-card new-environment)
        :do-function 'event-do-instruction
        :do-start :find-card
        :undo-start restart
        :undo-function 'event-undo-instruction))
    
    restart-ask-new
    (setf new-environment (list (cons *status-key* "OK")))
    (setf new-environment (match (third value) (question-column current-question) new-environment))
    (assert (not (eq new-environment :fail)))
    
    (setf new-environment (match (fourth value) (question-seek current-question) new-environment))
    
    (return-from %%execute
      (make-event
        :class :instruction
        :do-data (list current-card new-environment)
        :do-start :ask-user-card
        :do-function 'event-do-ask
        :undo-start restart
        :undo-function 'event-undo-ask))
          
    restart-ask-instruction-card
    (go restart-answer-no)
    
    restart-next-question
    ;; i.e. Step 5
    (setf current-ask (first (state-current-asks state)))
    (if (null (ask-steps current-ask))
        (return-from %%execute
          (make-event
            :class :end-question
            :do-start :complete
            :undo-start restart))
        (return-from %%execute
          (make-event
            :class :continue-instruction-card
            :do-start :continue-instruction-card
            :undo-start restart)))
    
    restart-continue-instruction-card
    (setf current-ask (first (state-current-asks state)))
    (setf current-question (first (ask-steps current-ask)))
    ;; i.e. Step 8
    (loop for link in (rest (rest (member (ask-tag current-ask) (ask-body current-ask)
                                    :test #'(lambda (value1 value2)
                                              (and (stringp value2)
                                                   (pro:whole-string-equal value1 value2))))))
          if (listp link)
            do (cond ((pro:whole-string-equal (first link) "If")
                      (if (not (eq :fail (match (second link) (rest (assoc *status-key* (ask-environment current-ask))) ())))
                          (return-from %%execute
                            (make-event
                              :class :instruction-step
                              :do-start :find-card
                              :do-data (list (third link))
                              :do-function 'event-do-instruction-step
                              :undo-start restart
                              :undo-function 'event-undo-instruction-step))))
                     (t
                      (error 'hank-error
                        :format-control "Illegal special form in instruction card")))
          else
            do (loop-finish))
    (return-from %%execute
      (make-event
        :class :instruction-end
        :do-start :complete
        :undo-start restart))

    restart-answer-no
    ;; i.e. Step 6b
    (return-from %%execute
      (make-event
        :class :answer-fail
        :do-start :next-question
        :do-function 'event-do-answer-fail
        :undo-start restart
        :undo-function 'event-undo-answer-fail))
    
    restart-complete
    (return-from %%execute
      (make-event
        :class :complete
        :do-function 'event-do-complete
        :do-start :continue
        :undo-start restart
        :undo-function 'event-undo-complete))
    
    restart-continue
    ;; i.e. Step 10
    (if (setf value (first (rest (state-current-asks state))))
        (return-from %%execute
          (make-event
            :class :instruction-return
            :do-start :continue-instruction-card
            :do-data ()
            :do-function 'event-do-instruction-return
            :undo-start restart
            :undo-function 'event-undo-instruction-return))
        (return-from %%execute nil))
    ))

