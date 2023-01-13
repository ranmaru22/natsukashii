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
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "config" :depends-on ("package"))
                 (:file "utils" :depends-on ("config"))
                 (:file "scraper-old" :depends-on ("utils"))
                 (:file "scraper-new" :depends-on ("utils"))))))
