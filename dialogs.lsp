;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Authors: Stuart Watt
;;;          The Open University

(in-package "HANK")

;;; Some dialogs that we need for the system. The idea is that we 
;;; use these in a few places in the OCML that depend on using the
;;; interface system. We don't use y-or-n-p because these are breaking
;;; in the runtime system because of package problems, and also because
;;; of Franz' persistence in adhering to the various grey limits of
;;; the system. Our equivalent is rather similar but will, I hope, be
;;; a bit closer to the Windows standards.
;;;
;;; Choosing a font. This should happen at run time, depending on
;;; the system that we're running. In fact, a lot should depend on the
;;; system we're running.

(defvar *message-font* ())

(defun initialise-message-font ()
  (setf *message-font*
    #+(and ACLPC HANK-WIN95-LOOK) (cg:make-font-ex nil :ms\ sans\ serif 10)
    #+(and ACLPC (not HANK-WIN95-LOOK)) (cg:make-font-ex nil nil 10)
    #+Procyon-Common-Lisp (cg:make-font nil nil 10)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (initialise-message-font))
   
(unless (member 'initialise-message-font acl::*system-init-fns*)
  (setf acl::*system-init-fns* (nconc acl::*system-init-fns*
                                      '(initialise-message-font))))

(defun y-or-n-p (&optional format-string &rest args)
  (eql pc::IDYES
       (win::MessageBox 
         (pc::window-handle (pc::top-level-window (pc::selected-window-or-screen)))
         (if format-string (printer::format1 nil format-string args) "Are you sure?") 
         "" 
         #.(logior pc::MB_ICONQUESTION pc::MB_YESNO pc::MB_APPLMODAL))))

;;; A better solution will be to use a more complete dialog manager which offers
;;; a bigger range of buttons. The main point here is that I want to be able to
;;; use the "Yes to all" option for some operations, and this isn't available 
;;; (at least, not obviously) through the message box system built into Windows.

(defconstant dialog-icon
   (cg:make-dialog-item 
      :widget 'cg:static-picture
      :value cg:error-icon
      :name :icon
      :box '#.(cg:make-box 8 6 30 30)
      :background-color *dialog-background-colour*
      :border :none
      :dialog-units-p t))

(defconstant dialog-static-text
  (cg:make-dialog-item 
    :widget 'cg:static-text
    :value ""
    :font *message-font*
    :name :message
    :box '#.(cg:make-box 38 6 152 22)
    :background-color *dialog-background-colour*
    :dialog-units-p t))

(defconstant dialog-default-button
  (cg:make-dialog-item
    :title "Default"
    :widget 'cg:default-button
    :box '#.(cg:make-box 10 38 50 50)
    :dialog-units-p t
    :name :default
    :tabstop t :groupstart t
    :background-color *dialog-background-colour*
    :font *message-font*
    :set-value-fn 'cg:button-return))

(defconstant dialog-cancel-button
   (cg:make-dialog-item
      :title "Cancel"
      :widget 'cg:cancel-button
      :box '#.(cg:make-box 110 38 150 50)
      :dialog-units-p t
      :name :cancel
      :background-color *dialog-background-colour*
      :font *message-font*
      :set-value-fn 'cg:button-return))
   
(defconstant dialog-button2
   (cg:make-dialog-item
      :title "Button2"
      :widget 'cg:button
      :box '#.(cg:make-box 60 38 100 50)
      :dialog-units-p t
      :name :button2
      :background-color *dialog-background-colour*
      :font *message-font*
      :set-value-fn 'cg:button-return))
   
(defconstant dialog-button3
   (cg:make-dialog-item
      :title "Button3"
      :widget 'cg:button
      :name :button3
      :box '#.(cg:make-box 60 38 100 50)
      :dialog-units-p t
      :background-color *dialog-background-colour*
      :font *message-font*
      :set-value-fn 'cg:button-return))

(defvar *dialog* nil)

(pushnew '*dialog* acl::*system-session-variables*)

(defun dialog (window)
   (let ((top-level-window (pc::top-level-window window)))
      (or 
        ;; return NIL if not a dialog or not owned by the right top level window
        (pc::verify-pop-up-dialog-parent *dialog* top-level-window)
        ;; need to open a new one
        (progn
             (setup-dialog-gaps top-level-window)
             (setq *dialog*
                (cg:open-dialog 
                   '#.(list dialog-default-button dialog-button2 
                            dialog-button3 dialog-cancel-button 
                            dialog-icon dialog-static-text )
                   'cg:dialog
                   top-level-window
                   :window-border :dialog-box
                   :background-color *dialog-background-colour*
                   :window-interior #.(cg:make-box 100 100 200 200)
                   :pop-up-p t
                   :title ""))))))

(defvar *dialog-hgap* 
   ;; in pixels
   16)

(defvar *dialog-vgap* 
   ;; in pixels
   16)

(defun setup-dialog-gaps (window)
  (let ((box (cg:dialog-to-screen-units window (cg:nmake-box #.(cg:make-box 0 0 0 0) 0 0 8 8))))
    (setq *dialog-hgap* (cg:box-width box))
    (setq *dialog-vgap* (cg:box-height box))))

(defun update-dialog (title prompt icon button1 button2 button3 button4)
   ;; set the title
   (cg:set-stream-title *dialog* title)
   (let ((icon-window 
            (may-hide-item dialog-icon icon #'cg:set-dialog-item-value))
         (prompt-window 
            (cg:dialog-item-window dialog-static-text))
         (default-button-window 
            (may-hide-item dialog-default-button button1))
         (cancel-button-window
            (may-hide-item dialog-cancel-button 
               (or button4 button3 button2)))
         (button2-window
            (may-hide-item dialog-button2
               (and button3 button2)))
         (button3-window
            (may-hide-item dialog-button3
             (and button4 button3))))
      ;; now update the prompt     
      (cg:set-dialog-item-value dialog-static-text prompt)
      ;; update the window size
      (multiple-value-bind (prompt-width prompt-height)
          (size *dialog* prompt)
         (let* ((prompt-window-exterior
                 (cg:nmake-box-relative #.(cg:make-box 0 0 0 0)
                    (acl:i+ *dialog-hgap*
                       (if icon-window 
                          (window-exterior-right icon-window) 
                          0))
                    *dialog-vgap*
                    prompt-width prompt-height))
                (button-top 
                 (acl:i+ 
                    (acl:imax (cg:box-bottom prompt-window-exterior)
                       (if icon-window 
                          (window-exterior-bottom icon-window)
                          0))
                    *dialog-vgap*))
                (button-count
                 (cond
                    (button4 4)
                    (button3 3)
                    (button2 2)
                    (t 1)))
                (buttons-min-right
                 ;; minimum right most end of the buttons
                 (acl:i* button-count 
                    (acl:i+ (cg:window-exterior-width default-button-window)
                       *dialog-hgap*)))
                (buttons-left-extra
                 ;; center buttons
                 (acl:imax 0
                    (acl:i/ 
                       (acl:i- (cg:box-right prompt-window-exterior)
                          buttons-min-right)
                       2)))
                (last-right 0))
            ;; fix the prompt window
            (cg:reshape-window-exterior prompt-window prompt-window-exterior)
            ;; move the buttons
            (pc::move-window-x-y default-button-window 
               (acl:i+ buttons-left-extra *dialog-hgap*)
               button-top)
            (setq last-right 
               (window-exterior-right default-button-window))
            (when button2-window
               (pc::move-window-x-y button2-window 
                  (acl:i+ *dialog-hgap* last-right) button-top)
               (setq last-right 
                  (window-exterior-right button2-window)))
            (when button3-window
               (pc::move-window-x-y button3-window 
                  (acl:i+ *dialog-hgap* last-right) button-top)
               (setq last-right 
                  (window-exterior-right button3-window)))
            (when cancel-button-window
               (pc::move-window-x-y cancel-button-window 
                  (acl:i+ *dialog-hgap* last-right) button-top)
               (setq last-right 
                  (window-exterior-right cancel-button-window)))
            ;; expand the dialog
            (cg:reshape-window-interior *dialog* 
               (cg:center-box-on-screen
                  (acl:i+ *dialog-hgap*
                     (acl:imax last-right (cg:box-right prompt-window-exterior)))
                  (acl:i+ *dialog-vgap* 
                     (window-exterior-bottom default-button-window))))
]
   
(defun window-exterior-left (window)
   (cg:box-left (cg:nwindow-exterior window #.(cg:make-box 0 0 0 0]
   
(defun window-exterior-right (window)
   (cg:box-right (cg:nwindow-exterior window #.(cg:make-box 0 0 0 0]
   
(defun window-exterior-bottom (window)
   (cg:box-bottom (cg:nwindow-exterior window #.(cg:make-box 0 0 0 0]
   
(defun move-window-x-y (window x y)
   (cg:move-window window (cg:nmake-position #.(cg:make-position 0 0) x y]

(defun may-hide-item (item title &optional (setter #'cg:set-dialog-item-title))
   ;;  If title is NIL, hides the dialog-item window and returns nil,
   ;;  else shows the window and sets its title and returns it.
   (let ((window (cg:dialog-item-window item)))
      (if title 
         (progn
            (funcall setter item title)
            (cg:expand-window window)
            window)
         (progn
            (cg:shrink-window window t)
            nil))))

(defun size (dialog prompt)
   (let ((width 0) (height (cg:line-height dialog)) (start 0) (end (length (the string prompt))))
      (acl:for newline-index acl::bound nil 
         acl:while (acl:i> end start)
         do
         (setq newline-index 
            (or (position #\Newline prompt :start start :end end) end))
         (setq width 
            (acl:imax width 
               (cg:stream-string-width dialog prompt start newline-index)))
         (when (acl:i< newline-index end) (acl:iincf height (cg:line-height dialog)))
         (setq start (acl:i1+ newline-index)))
      (values width height)))

(defun pop-up-message-dialog (stream title prompt icon
                              button1 &optional button2 button3 button4)
   ;; always let the mouse go, in case we are trying to signal an error within a drag
   (cg:release-mouse stream)
   (let* ((dialog (dialog stream)))
     (update-dialog title prompt icon button1 button2 button3 button4)
     (acl:select (cg:pop-up-dialog dialog)
       (dialog-default-button 1)
       (dialog-cancel-button
         (cond
           (button4 4)
           (button3 3)
           (t 2)))
       (dialog-button2 2)
       (dialog-button3 3))))

;;; We can use this for the various commands that we need. In particular,
;;; the main one we need at this stage is the one that allows for the basic
;;; choice at the yes-or-no-p stage.

(defun yes-to-all-or-no-p (title message &rest arguments)
  (let ((window (pc::top-level-window (pc::selected-window-or-screen))))
    (ecase (pop-up-message-dialog window title (apply #'format nil message arguments)
             cg:warning-icon "~Yes" "Yes to ~All" "~Cancel")
      (1 t)
      (2 :all)
      (3 nil))))

(acl::defbparameter *string-dialog* nil)

(acl::pushnewq '*string-dialog* acl::*system-session-variables*)

;;;; Opening the string dialog

(defun string-dialog (window)
   (let ((top-level-window (cg:top-level-window window)))
      (or 
          ;; return NIL if not a dialog or not owned by the right top level 
          ;; window  
          (pc::verify-pop-up-dialog-parent *string-dialog* top-level-window)
          (progn
             (setup-dialog-gaps top-level-window)
             (setq *string-dialog* 
                (cg:open-dialog 
                   (list
                      (cg:make-dialog-item :widget 'cg:static-picture 
                         :background-color t ;; <1>
                         :box '#.(cg:make-box-relative 8 6 30 30)
                         :dialog-units-p t :name :icon)
                      (cg:make-dialog-item :widget 'cg:static-text 
                         :background-color t
                         :box '#.(cg:make-box-relative 8 6 174 16)
                         :dialog-units-p t :name :prompt)
                      (cg:make-dialog-item :widget 'cg:editable-text 
                         :background-color t
                         :border :black
                         :box '#.(cg:make-box-relative 8 26 174 12) :dialog-units-p t 
                         :name :string)
                      (cg:make-dialog-item :widget 'cg:default-button 
                         :box '#.(cg:make-box-relative 94 44 40 12) :dialog-units-p t
                         :set-value-fn 'cg:button-return :name :button1
                         :tabstop t :groupstart t)
                      (cg:make-dialog-item :widget 'cg:cancel-button 
                         :box '#.(cg:make-box-relative 142 44 40 12) :dialog-units-p t
                         :set-value-fn 'cg:button-return :name :button2))
                   'cg:dialog top-level-window 
                   :pop-up-p t :title "" 
                   :background-color *dialog-background-colour*
                   :window-interior (cg:center-box-on-screen 190 55 t)
                   :dialog-units-p t))))))

;;;; ENTRY POINTS

;;; Returns 3 values: string1, string2, option chosen

(defun show-strings-dialog (dialog title prompt icon string button1 button2)
   (if (not (eq (cg:window-state dialog) :shrunk))
      ;; already in use
      (values string 2)
      (let* ((icon-window  
              (may-hide-item (cg:find-named-object :icon dialog) icon #'cg:set-dialog-item-value))
             (button1-item (cg:find-named-object :button1 dialog))
             (button1-window (cg:dialog-item-window button1-item))
             (button2-item (cg:find-named-object :button2 dialog))
             (button2-window (cg:dialog-item-window button2-item))
             (prompt-item (cg:find-named-object :prompt dialog))
             (prompt-window (cg:dialog-item-window prompt-item))
             (string-item (cg:find-named-object :string dialog))
             (string-window (cg:dialog-item-window string-item)))
         (cg:set-stream-title dialog title)
         (cg:set-dialog-item-title button1-item button1)
         (cg:set-dialog-item-title button2-item button2)
         (cg:set-dialog-item-value prompt-item prompt)
         (cg:set-dialog-item-value string-item string)
         ;; resize items
         (let* ((prompt-exterior 
                 (cg:nwindow-exterior prompt-window #.(cg:make-box 0 0 0 0)))
                (last-bottom
                 (acl:imax
                    (if icon-window (pc::window-exterior-bottom icon-window) 0)
                    (acl:i+ (cg:box-top prompt-exterior)
                       (nth-value 1 (size dialog prompt))))))
            ;; prompt
            (cg:set-box-left prompt-exterior 
               (acl:i+ *dialog-hgap*
                  (if icon-window (pc::window-exterior-right icon-window) 0)))
            (cg:set-box-bottom prompt-exterior last-bottom)
            (cg:reshape-window-exterior prompt-window prompt-exterior)
            ;; string
            (cg:move-window string-window
               (cg:nmake-position #.(cg:make-position 0 0) 
                  (pc::window-exterior-left string-window)
                  (acl:i+ *dialog-vgap* last-bottom)))
            (setq last-bottom (pc::window-exterior-bottom string-window))
            ;; buttons
            (cg:move-window button1-window 
               (cg:nmake-position #.(cg:make-position 0 0) 
                  (pc::window-exterior-left button1-window)
                  (acl:i+ *dialog-vgap* last-bottom)))
            (cg:move-window button2-window 
               (cg:nmake-position #.(cg:make-position 0 0) 
                  (pc::window-exterior-left button2-window)
                  (acl:i+ *dialog-vgap* last-bottom)))
            (setq last-bottom (pc::window-exterior-bottom button2-window))
            ;; the window
            (cg:reshape-window-interior dialog
               (cg:center-box-on-screen
                  (cg:window-interior-width dialog)
                  (acl:i+ (acl:i/ (acl:i* *dialog-vgap* 3) 4)
                     (pc::window-exterior-bottom button2-window))))               
            ;; show the dialog
            (let ((option 
                   (if (typep (cg:pop-up-dialog dialog) 'cg:cancel-button) 2 1)))
               (values 
                  (cg:dialog-item-value string-item)
                  option))))))
   
(defun ask-user-for-string (prompt string1 option1 option2 &optional title) ;; <2>
  (multiple-value-bind (string1 option-no)
      (show-strings-dialog (string-dialog (pc::selected-window-or-screen))
         (or title "") ;; <2>
          prompt nil string1 option1 option2)
    (values string1 option-no)))

