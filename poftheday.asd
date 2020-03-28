(asdf:defsystem "poftheday"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :serial t
  :defsystem-depends-on ("literate-lisp")
  :depends-on ("cl-org-mode"
               "cl-bootstrap"
               "cl-fad"
               "str"
               "cl-ppcre"
               "cl-who"
               "ppath"
               "alexandria"
               "xml-emitter"
               "doplus")
  :components ((:org "readme")))
