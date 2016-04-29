;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For additional details, comments and documentation, see the main code
;;; file "gittest.lisp"
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem #:gittest
  :name "gittest"
  :description "Gittest is a little project to call git from the CL REPL"
  :author "Damien John Melksham"
  :license "All rights reserved"
  :serial t
  :components ((:file "package")
               (:file "gittest"))
  :depends-on (#:uiop #:inferior-shell))

