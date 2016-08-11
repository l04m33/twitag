(asdf:defsystem #:twitag
  :description "Yet another twitter bot"
  :author "Kay Z. <l04m33@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-async
               #:cl-async-twitter
               #:blackbird
               #:sqlite
               #:lparallel
               #:vom)
  :components ((:module "src"
                :pathname "src"
                :components ((:file "package")
                             (:file "db" :depends-on ("package"))
                             (:file "twitag" :depends-on ("package" "db")))))
  :in-order-to ((test-op (test-op #:twitag-test))))
