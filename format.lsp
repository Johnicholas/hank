;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; Hank stuff for changing the drawing format.  This is basically just another
;;; dialog window, but it should link to the graph page system as well.  

(acl::defbparameter *format-dialog* nil)

(acl::pushnewq '*format-dialog* acl::*system-session-variables*)

;;;; Opening the format dialog

(defun format-dialog (window)
   (let ((top-level-window (cg:top-level-window window)))
      (or 
          ;; return NIL if not a dialog or not owned by the right top level 
          ;; window  
          (pc::verify-pop-up-dialog-parent *format-dialog* top-level-window)
          (progn
             (setq *format-dialog* 
                (cg:open-dialog 
                   (list
                      (cg:make-dialog-item :widget 'cg:static-text 
                         :background-color t
                         :box '#.(cg:make-box-relative 8 20 40 11)
                         :font *message-font*
                         :value "Pages across:"
                         :dialog-units-p t)
                      (cg:make-dialog-item :widget 'cg:lisp-text 
                         :background-color cg:white
                         :border :plain
                         :range '(integer 1 10)
                         :box '#.(cg:make-box-relative 50 18 15 11) :dialog-units-p t 
                         :value 1
                         :font *message-font*
                         :name :pages-across)
                      (cg:make-dialog-item :widget 'cg:static-text 
                         :background-color t
                         :box '#.(cg:make-box-relative 8 33 40 11)
                         :font *message-font*
                         :value "Pages down:"
                         :dialog-units-p t)
                      (cg:make-dialog-item :widget 'cg:lisp-text 
                         :background-color cg:white
                         :border :plain
                         :range '(integer 1 10)
                         :box '#.(cg:make-box-relative 50 31 15 11) :dialog-units-p t 
                         :value 1
                         :font *message-font*
                         :name :pages-down)
                      (cg:make-dialog-item :widget 'cg:default-button 
                         :box '#.(cg:make-box-relative 94 53 40 11) :dialog-units-p t
                         :set-value-fn 'cg:button-return :name :OK
                         :title "OK"
                         :font *message-font*
                         :tabstop t :groupstart t)
                      (cg:make-dialog-item :widget 'cg:cancel-button 
                         :box '#.(cg:make-box-relative 142 53 40 11) :dialog-units-p t
                         :font *message-font*
                         :title "Cancel"
                         :set-value-fn 'cg:button-return :name :cancel)
                      (cg:make-dialog-item :widget 'cg:group-box 
                         :background-color t
                         :box '#.(cg:make-box-relative 4 6 65 40)
                         :font *message-font*
                         :title "Drawing Size"
                         :dialog-units-p t))
                   'cg:dialog top-level-window 
                   :pop-up-p t :title "Drawing Format" 
                   :background-color *dialog-background-colour*
                   :window-interior (cg:center-box-on-screen 186 68 t)
                   :dialog-units-p t))))))

(comtab:defmenu-command :drawing-format ((window graph))
  (let* ((main-window (application-window-main-window (graph-editor-window window)))
         (dialog (format-dialog main-window)))
    (setf (cg:dialog-item-value (cg:find-named-object :pages-across dialog)) (cg:position-x (graph-pages window)))
    (setf (cg:dialog-item-value (cg:find-named-object :pages-down dialog)) (cg:position-y (graph-pages window)))
    (let ((button (cg:pop-up-dialog dialog)))
      (when (and button (eq (cg:object-name button) :OK))
        (setf (graph-pages window)
                (cg:make-position
                  (cg:dialog-item-value (cg:find-named-object :pages-across dialog))
                  (cg:dialog-item-value (cg:find-named-object :pages-down dialog))))))))


        