;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

(defmethod cg:about-to-show-menu ((window main-window) menu)
  (case (cg:object-name menu)
    ((:file)
     (multiple-value-bind (savep save-as-p) (:saveablep window)
       (cg:set-menu-item-available-p (cg:find-named-object :save menu) savep)
       (cg:set-menu-item-available-p (cg:find-named-object :save-as menu) save-as-p)
       (cg:set-menu-item-available-p (cg:find-named-object :revert menu) savep))
     (multiple-value-bind (printablep printable-selection-p) (:printablep window)
       (cg:set-menu-item-available-p (cg:find-named-object :page-setup menu) nil)
       (cg:set-menu-item-available-p (cg:find-named-object :drawing-format menu) printablep)
       (cg:set-menu-item-available-p (cg:find-named-object :print menu) printablep))
     (multiple-value-bind (exportablep exportable-selection-p) (:exportablep window)
       (cg:set-menu-item-available-p (cg:find-named-object :export menu) exportablep)
       (cg:set-menu-item-available-p (cg:find-named-object :export-selection menu) 
         (and exportablep
              exportable-selection-p
              (multiple-value-bind (window selection) (:selection window) selection))))
     (cg:set-menu-item-available-p (cg:find-named-object :close menu) (:closablep window)))
    ((:edit)
     (multiple-value-bind (label availablep) (get-undo-label window)
       (let ((item (cg:find-named-object :undo menu)))
         (cg:set-menu-item-title item label)
         (cg:set-menu-item-available-p item availablep)))
     (multiple-value-bind (label availablep) (get-redo-label window)
       (let ((item (cg:find-named-object :redo menu)))
         (cg:set-menu-item-title item label)
         (cg:set-menu-item-available-p item availablep)))
     (multiple-value-bind (window selection) (:selection window)
       (selection-show-menu :edit window menu selection)))
    ((:cards)
     (multiple-value-bind (window selection) (:selection window t)
       (selection-show-menu :cards window menu selection)))
    ((:ask)
     (multiple-value-bind (window selection) (:selection window t)
       (if (or (null selection)
               (stringp selection))
           (progn
             (cg:set-menu-item-title (cg:find-named-object :ask-briefly menu) "Ask Briefly...")
             (cg:set-menu-item-title (cg:find-named-object :ask-in-full menu) "Ask in Full..."))
           (progn
             (cg:set-menu-item-title (cg:find-named-object :ask-briefly menu) "Ask Briefly")
             (cg:set-menu-item-title (cg:find-named-object :ask-in-full menu) "Ask in Full"))))
     (let ((items (cg:get-stream-prop (get-workspace-window window) 'control-window)))
       (loop for name in '(:run-back :step-back :stop :step :run)
             do (cg:set-menu-item-available-p (cg:find-named-object name menu)
                  (cg:dialog-item-available-p (cg:find-named-object name items))))))
    ((:window)
     (when (windows-mismatch-menu-p window menu)
       (place-all-windows-on-menu window menu)
       (select-changed-windows menu))))
  (call-next-method))

(defun visible-not-special-window-p (window menu)
  (and (typep window 'cg:window)
       (not (eq :shrunk (cg:window-state window)))
       (not (member (cg:object-name window) (cg:get-stream-prop menu 'special-items)
              :key #'cg:menu-item-name))))

(defun place-all-windows-on-menu (main-window menu)
  (cg:update-menu menu
    (append (cg:get-stream-prop menu 'special-items)
            (loop for window in (all-windows main-window)
                  for firstp = t then nil
                  for index = (window-index window main-window)
                  if firstp
                    collect cg:menu-separator
                  end
                  collect (cg:set-stream-prop window 'menu-item 
                            (cg:make-menu-item :title 
                              (format () "~:[~;~~~]~D ~A" (<= index 9) index (cg:stream-title window))
                              :value window))))))

(defun windows-mismatch-menu-p (main-window menu)
  (or (notevery #'(lambda (window)
                    (visible-not-special-window-p window menu))
        (the list (cg:menu-items menu)))
      (notevery #'(lambda (window)
                    (if (visible-not-special-window-p window menu)
                        (let ((menu-item (cg:get-stream-prop window 'menu-item)))
                          (and menu-item (eq (cg:menu-item-title menu-item) (cg:stream-title window))))
                        (progn
                          (cg:set-stream-prop window 'menu-item nil)
                          t)))
        (the list (cg:windows (main-window-body main-window))))))

(defun select-changed-windows (menu)
  (loop for menu-item in (cg:menu-items menu) do
    (cg:set-menu-item-selected-p menu-item (:modified-p (cg:menu-item-value menu-item)))))

;;; Paste should only be valid if the item can be pasted as a graph element. This
;;; means that conversion should be possible. If the top item can't be converted into
;;; a graph element, forget being able to paste. This will be flagged if the top item
;;; isn't marked properly as a term. 

(defmethod selection-show-menu ((name (eql :edit)) window menu selection) 
  (cg:set-menu-item-available-p (cg:find-named-object :delete menu) selection)
  (cg:set-menu-item-available-p (cg:find-named-object :duplicate menu) (and selection 
                                                                            (listp selection)))
  (cg:set-menu-item-available-p (cg:find-named-object :cut menu) selection)
  (cg:set-menu-item-available-p (cg:find-named-object :copy menu) selection))

(defun make-file-menu-items ()
  (list (cg:make-menu-item :title "New" :name :new :value :new
          :available-p t :event-synonym '(cg:control-key #\N))
        (cg:make-menu-item :title "Open..." :name :open :value :open
          :available-p t :event-synonym '(cg:control-key #\O))
        #.cg:menu-separator        
        (cg:make-menu-item :title "Close" :name :close :value :close 
          :available-p t :event-synonym 
          #+Windows '(cg:control-key pc:vk-f4) #+Macintosh '(cg:control-key #\W))
        (cg:make-menu-item :title "Save" :name :save :value :save 
          :available-p t :event-synonym '(cg:control-key #\S))
        (cg:make-menu-item :title "Save As..." :name :save-as :value :save-as 
          :available-p t :event-synonym ())
        (cg:make-menu-item :title "Revert to Saved" :name :revert :value :revert 
          :available-p t :event-synonym ())
        #.cg:menu-separator
        (cg:make-menu-item :title "Scripts" 
          :name :scripts
          :value (cg:open-menu (list (cg:make-menu-item :title "~Play Script..." :name :play :value :play 
                                       :available-p t :event-synonym ())
                                     (cg:make-menu-item :title "~Real Time" :name :real-time-p :value :real-time-p 
                                       :available-p t :selected-p t :event-synonym ())
                                     #.cg:menu-separator
                                     (cg:make-menu-item :title "~Start Recording..." :name :record :value :record 
                                       :available-p t :event-synonym ())
                                     #.cg:menu-separator
                                     (cg:make-menu-item :title "~Execute Command..." :name :execute :value :execute 
                                       :available-p t :event-synonym '(cg:control-key #\E)))
                   'cg:pull-down-menu cg:*screen*
                   :selection-function #'(lambda (menu item main-window)
                                           (funcall (cg:menu-item-value item) main-window))
                   :name :scripts-menu)
          :available-p t)
        #.cg:menu-separator
        (cg:make-menu-item :title "Export..." :name :export 
          :value :export
          :available-p t :event-synonym ())
        (cg:make-menu-item :title "Export Selection..." :name :export-selection 
          :value :export-selection
          :available-p t :event-synonym ())
        #.cg:menu-separator
        (cg:make-menu-item :title "Drawing Format..." :name :drawing-format :value :drawing-format 
          :available-p t :event-synonym nil)
        (cg:make-menu-item :title "Page Setup..." :name :page-setup 
          :value :page-setup
          :available-p t :event-synonym ())
        (cg:make-menu-item :title "Print..." :name :print :value :print 
          :available-p t :event-synonym '(cg:control-key #\P))
        #.cg:menu-separator
        (cg:make-menu-item :title #+Macintosh "Quit" #+Windows "Exit" :name :exit :value :exit
          :available-p t :event-synonym 
          #+Windows '(cg:alt-key pc:vk-f4) #+Macintosh '(cg:control-key #\Q))))

(defmethod open-menu ((window main-window) (menu (eql :file)) menu-bar)
  (cg:open-menu (make-file-menu-items) 'cg:pull-down-menu menu-bar 
    :pop-up-p t
    :title "~File"
    :name :file
    :selection-function 'top::toploop-do-menu-command))

(defun get-menu (window menu menu-bar)
  (or (getf (getf (cg:stream-plist window) 'menus) menu)
      (setf (getf (getf (cg:stream-plist window) 'menus) menu)
              (open-menu window menu menu-bar))))

(defun make-edit-menu-items ()
  (list (cg:make-menu-item :title "~Undo" :name :undo :value :undo 
          :available-p t :event-synonym '(cg:control-key #\Z))
        (cg:make-menu-item :title "~Redo" :name :redo :value :redo 
          :available-p t :event-synonym '(cg:control-key #\Y))
        #.cg:menu-separator
        (cg:make-menu-item :title "Cu~t" :name :cut :value :cut 
          :available-p t :event-synonym '(cg:control-key #\X))
        (cg:make-menu-item :title "~Copy" :name :copy :value :copy 
          :available-p t :event-synonym '(cg:control-key #\C))
        (cg:make-menu-item :title "~Paste" :name :paste :value :paste 
          :available-p t :event-synonym '(cg:control-key #\V))
        (cg:make-menu-item :title #+Windows "~Delete" #+Macintosh "Clear" :name :delete :value :delete 
          :available-p t :event-synonym #+Windows 'pc:vk-delete #+Macintosh nil)
        #+Windows
        (cg:make-menu-item :title "~Insert" :name :insert :value :insert 
          :available-p t :event-synonym #+Windows 'pc:vk-insert #+Macintosh nil)
        #.cg:menu-separator
        (cg:make-menu-item :title "Duplicat~e" :name :duplicate :value :duplicate 
          :available-p t :event-synonym '(cg:control-key #\D))
        #.cg:menu-separator
        (cg:make-menu-item :title "~Select All" :name :select-all :value :select-all 
          :available-p t :event-synonym '(cg:control-key #\A))))

(defmethod open-menu ((window main-window) (menu (eql :edit)) menu-bar)
  (cg:open-menu (make-edit-menu-items) 'cg:pull-down-menu menu-bar 
    :pop-up-p t
    :title "~Edit"
    :name :edit
    :selection-function 'top::toploop-do-menu-command))

;;; A lot of these should be changed. We want the menus to work slightly
;;; differently, using New Fact Card, and so on, and Change Card Type as a
;;; general hierarchical menu to change the type of card elements. This
;;; is a bit different to the current strategy, but it is probably a bit
;;; easier for people to understand what they can do. 

(defun make-element-menu-items (type)
  (loop for item in *element-types*
        when (eq (element-type-type item) type)
        collect (cg:make-menu-item :title (cl:format () "New ~A ~:(~A~)" (element-type-label item) type)
                                   :name (element-type-class item)
                                   :plist (list :arguments (list :type (element-type-class item)))
                                   :value :new-element
                                   :available-p t :event-synonym nil)))

(defun make-change-type-menu (type name)
  (cg:open-menu (loop for item in *element-types*
                      when (eq (element-type-type item) type)
                      collect (cg:make-menu-item :title (element-type-label item)
                                                 :name (element-type-class item)))
    'cg:pull-down-menu cg:*screen*
    :name name))

(defun make-cards-menu-items (&aux card-menu box-menu items)
  (setf card-menu (make-change-type-menu :card :change-type-cards))
  (setf box-menu (make-change-type-menu :box :change-type-boxes))
  `(,@(setf items (make-element-menu-items :card))
    ,@(when (rest items)
        (list (cg:make-menu-item :title "~Change Card Type" 
                                 :name :change-type-cards
                                 :value card-menu 
                                 :available-p t :event-synonym nil)))
    ,cg:menu-separator
    ,@(setf items (make-element-menu-items :box))
    ,@(when (rest items)
        (cg:make-menu-item :title "~Change Box Type" 
                           :name :change-type-boxes 
                           :value box-menu 
                           :available-p t :event-synonym nil))
    ,cg:menu-separator
    ,(cg:make-menu-item :title "Label Link \"OK\"" :name :label-link-ok :value :label-link-ok)
    ,(cg:make-menu-item :title "Label Link \"Fail\"" :name :label-link-fail :value :label-link-fail)
    ,(cg:make-menu-item :title "Unlabel Link" :name :unlabel-link :value :unlabel-link)))

(defmethod open-menu ((window main-window) (menu (eql :cards)) menu-bar)
  (cg:open-menu (make-cards-menu-items) 'cg:pull-down-menu menu-bar
    :selection-function #'(lambda (menu menu-item window)
                            (apply (cg:menu-item-value menu-item) window 
                              (getf (cg:menu-item-plist menu-item) :arguments)))
    :pop-up-p t
    :title "~Cards"
    :name :cards))

(defun make-ask-menu-items ()
  (list (cg:make-menu-item :title "Ask Briefly" :name :ask-briefly :value :ask-briefly :available-p t 
          :event-synonym '(cg:control-key #\K))
        (cg:make-menu-item :title "Ask in Full" :name :ask-in-full :value :ask-in-full :available-p t 
          :event-synonym '(cg:alt-key #\K))
        cg:menu-separator
        (cg:make-menu-item :title "Go Back" :name :run-back :value :run-back :available-p t
          :event-synonym '(cg:alt-key #\B))
        (cg:make-menu-item :title "Step Back" :name :step-back :value :step-back :available-p t 
          :event-synonym '(cg:control-key #\B))
        (cg:make-menu-item :title "Stop" :name :stop :value :stop :available-p t :event-synonym nil)
        (cg:make-menu-item :title "Step Forward" :name :step :value :step :available-p t 
          :event-synonym '(cg:control-key #\F))
        (cg:make-menu-item :title "Go Forward" :name :run :value :run :available-p t 
          :event-synonym '(cg:control-key #\G))
        cg:menu-separator
        (cg:make-menu-item :title "Clear Workspace" :name :clear-workspace :value :clear-workspace :available-p t :event-synonym nil)))

(defmethod open-menu ((window main-window) (menu (eql :ask)) menu-bar)
  (cg:open-menu (make-ask-menu-items) 'cg:pull-down-menu menu-bar 
    :pop-up-p t
    :title "~Ask"
    :name :ask
    :selection-function 'top::toploop-do-menu-command))

;;; We probably want a Windows menu, or something like it. This will be
;;; where we can add in any windows that we want. We can also show all the
;;; files with a check to show whether or not they've been saved since
;;; they were last changed. However, the title of the Windows menu should
;;; be carefully chosen. 

(defun make-help-menu-items ()
  (list (cg:make-menu-item :title "About Hank..." :name :about-application :value :about-application 
          :available-p t :event-synonym ())))

(defmethod open-menu ((window main-window) (menu (eql :help)) menu-bar)
  (cg:open-menu (make-help-menu-items) 'cg:pull-down-menu menu-bar 
    :pop-up-p t
    :title "~Help"
    :name :help
    :selection-function 'top::toploop-do-menu-command))

(defmethod open-menu ((window main-window) (menu (eql :window)) menu-bar &aux menu)
  (setf menu (cg:open-menu () 'cg:pull-down-menu menu-bar 
               :pop-up-p t
               :title "~Window"
               :name :window
               :selection-function #'(lambda (menu item window)
                                       (let ((value (cg:menu-item-value item)))
                                         (if (functionp value)
                                             (let ((window (funcall value window)))
                                               (setf (cg:menu-item-value item) window)
                                               (cg:select-window window))
                                             (cg:select-window value))))))
  (cg:set-stream-prop menu 'special-items 
    (list (cg:make-menu-item :title "Control Panel" 
                             #+Windows :event-synonym #+Windows 'pc:vk-f9
                             :name :control-panel-window
                             :value #'(lambda (window &rest keys)
                                        (or (cg:find-named-object :control-panel-window (main-window-body window))
                                            (apply #'cg:open-stream 'control-panel-window (main-window-body window) :io
                                              :name :control-panel-window
                                              :main-window window
                                              keys))))
          (cg:make-menu-item :title "Workspace" 
                             #+Windows :event-synonym #+Windows 'pc:vk-f10
                             :name :workspace-window
                             :value #'(lambda (window &rest keys)
                                        (or (cg:find-named-object :workspace-window (main-window-body window))
                                            (apply #'cg:open-stream 'workspace-window (main-window-body window) :io
                                              :name :workspace-window
                                              :main-window window
                                              :page-width 10000
                                              :page-height 10000
                                              keys))))
          #||
          ;; So far, I haven't had time to implement the rules window properly, so we're
          ;; eliminating the menu which opens it.
      
          (cg:make-menu-item :title "House Rules" 
                             #+Windows :event-synonym #+Windows 'pc:vk-f11
                             :name :house-rules-window
                             :value #'(lambda (window &rest keys)
                                        (or (cg:find-named-object :house-rules-window (main-window-body window))
                                            (apply #'cg:open-stream 'rules-window (main-window-body window) :io
                                              :name :house-rules-window
                                              :title "House Rules"
                                              :main-window window
                                              :font *rule-font*
                                              :rules (map 'vector #'identity (get-file-terms "c:\\stuart\\hank\\house.rul"))
                                              keys))))
          ||#
          ))
  (cg:update-menu menu (cg:get-stream-prop menu 'special-items))
  menu)

(defun find-named-menu-item (name menu-bar)
  (find name (cg:menu-items menu-bar)
    :key #'(lambda (item) (cg:object-name (cg:menu-item-value item)))))

(defmethod open-window-menus ((window main-window) menu-bar)
  (get-menu window :file menu-bar)
  (get-menu window :edit menu-bar)
  (get-menu window :cards menu-bar)
  (get-menu window :ask menu-bar)
  (get-menu window :window menu-bar)
  (get-menu window :help menu-bar)
   
  (cg:set-menu-item-available-p (find-named-menu-item :cards menu-bar) nil))

;;; Start up the interface neatly. We'll probably need to do more than this when we
;;; get around to handling startup documents and things like that. We should probably
;;; do all sorts of AppleEvent stuff, but this does run against portability. 

(defun top ()
  (loop for window = (cg:front-window cg:*screen*) then (cg:next-window window)
        do (when (null window)
             (return))
           (when (typep window 'main-window)
             (close window)))
  (let ((window (open-main-window)))
    (when *preference-status-bar-p*
      (cg:add-status-bar-to-window window))
    (cg:zoom-window window :maximized)
    (cg:select-window window)
    (let* ((menu (cg:window-menu window))
           (window-menu (cg:find-named-object :window menu)))
      (assert window-menu)
      (cg:about-to-show-menu window window-menu))
    window))

;;; This function is called in a runtime image. Basically, it opens the window and
;;; runs handling events. It will be terminated by the system calling pro:quit, 
;;; which happens when the main window is closed.

(defun runtime-top ()
  (let ((*package* (find-package "HANK")))
    (handler-bind ((error
                     #'(lambda (condition)
                         (cg:pop-up-message-dialog cg:*screen* "Serious Error"
                           (format () "Serious error found. Quitting because~%~A"
                             (write-to-string condition :escape nil)) cg:error-icon "OK")
                         (acl:quit :stop))))
      (top)
      (cg:process-events))))

(defun runtime-top-with-debugger ()
  (let ((*package* (find-package "HANK"))
        (*debugger-hook* #'(lambda (condition value)
                             (nonstandard-debugger condition)))
        (*top-level-window* nil))
    (declare (special *top-level-window*))
    (tagbody
      start
      (restart-bind ((abort #'(lambda ()
                                (go start))
                       :report-function #'(lambda (stream)
                                            (declare (special *expanded-restart-label-p*))
                                            (if *expanded-restart-label-p*
                                                (format stream "To stop whatever you're doing, but carry on using Hank, choose this option.")
                                                (format stream "")))))
        (unless *top-level-window* (setf *top-level-window* (top)))
        (cg:process-events)))))

(comtab:defmenu-command :undo ((window main-window))
  (record-command window "Undo")
  (let* ((window (:command-window window))
         (command (first (getf (cg:stream-plist window) 'undo-commands))))
    (cond ((null command))
          ((not (command-undoable-p command)))
          (t
           (record-command window "Undo")
           (command-undoit command)
           (pop (getf (cg:stream-plist window) 'undo-commands))
           (push command (getf (cg:stream-plist window) 'redo-commands))))))

(comtab:defmenu-command :redo ((window main-window))
  (record-command window "Redo")
  (let* ((window (:command-window window))
         (command (first (getf (cg:stream-plist window) 'redo-commands))))
    (cond ((null command))
          (t
           (record-command window "Redo")
           (command-redoit command)
           (pop (getf (cg:stream-plist window) 'redo-commands))
           (push command (getf (cg:stream-plist window) 'undo-commands))))))

;;; In practice, a lot of stuff which we'd otherwise put in the about to show
;;; menu stuff should probably be put here. It may be considerably faster
;;; that way, because it may not ever look at the menus at all, at least,
;;; not for many of the menus. 
;;; 
;;; We need a better way of handling this. Basically, the same option may get
;;; turned off and then on again if we do it this way. One solution is to delay
;;; the changes until we get an idle event, and then look at everything that
;;; needs to be done. Finally, closing the last window doesn't tell us of the
;;; change in selection status. 

(defmethod notify-window-selection ((window t) move-to-front-p)
  ())

(defun notify-close-window-selection (parent)
  (let ((front-window (cg:selected-window parent)))
    (when front-window
      (notify-window-selection front-window t))))

;;; Paradoxically, we need this for dialogs and stuff like that. When a dialog
;;; comes in and is closed, we need to detect the change. However, this does 
;;; affect the development environment, because the quit command now doesn't
;;; always quit. 

(defmethod cg:shrink-window ((window cg:dialog) option)
  (let ((parent (cg:stream-location window)))
    (notify-window-selection window nil)
    (call-next-method)
    (notify-close-window-selection parent)))

