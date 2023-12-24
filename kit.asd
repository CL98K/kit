(defsystem :kit
  :description "kit"
  :version "0.0.1"
  :author "CL98K <917599580@QQ.COM>"
  :licence ""
  :depends-on (:local-time :cl-csv :cxml :alexandria)
  :components ((:module "src"
                :serial t
                :components (
                             (:file "package")
                             (:file "printf")
                             (:file "parse")
                             (:file "number")
                             (:file "string")
                             ))))

(defsystem :kit/test
  :description "print formatted test"
  :version "1.0.0"
  :author "Gzz <x>"
  :licence ""
  :depends-on (:kit)
  :components ((:module "test"
                :components ((:file "top-test")
                             (:file "printf-test")
                             (:file "number-test")
                             (:file "string-test")))))
