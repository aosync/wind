(asdf:defsystem "wind"
  :description "LISP parsing playground"
  :serial t
  :depends-on ("tokbuf")
  :components ((:file "packages")
	       (:file "lex")))
