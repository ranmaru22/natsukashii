(asdf:defsystem "natsukashii"
  :name "natsukashii"
  :version "0.1"
  :class :package-inferred-system
  :author "Alex Sun"
  :licence "GPL-3.0-or-later"
  :description "Archive search tool for fanfiction.net"
  :serial t
  :depends-on (#:cl-ppcre
               #:str
               #:dexador
               #:quri
               #:plump
               #:lquery
               #:lparallel)
  :components ((:file "package")
               (:file "config")
               (:file "main")))
