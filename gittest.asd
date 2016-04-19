;;;; gittest.asd

(asdf:defsystem :gittest
  :description "Gittest is a little project so I can call git from within the CL REPL"
  :author "Damien John Melksham"
  :license "All rights reserved"
  :depends-on ("inferior-shell")
  :serial t
  :components ((:file "package")
               (:file "gittest"))
  :depends-on ("uiop"))
