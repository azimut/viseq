(asdf:defsystem "viseq"
  :author "azimut <azimut.github@protonmail.com>"
  :description "poor's man video sequencer"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:alexandria
               #:common-cv)
  :components ((:file "package")
               (:file "helpers")
               (:file "main")))
