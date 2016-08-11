(in-package #:cl-user)


(defpackage #:twitag/db
  (:use #:cl
        #:sqlite
        #:lparallel
        #:cl-async
        #:blackbird)
  (:shadowing-import-from #:cl-async #:delay)
  (:shadowing-import-from #:blackbird #:chain #:promise)
  (:export #:init-db
           #:close-db
           #:with-db
           #:add-user-tag
           #:get-user-tags
           #:remove-user-tag
           #:remove-all-user-tags
           #:get-tagged-users))


(defpackage #:twitag
  (:use #:cl
        #:cl-async
        #:cl-async-twitter
        #:blackbird
        #:twitag/db)
  (:export #:main))
