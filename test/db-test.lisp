(in-package #:cl-user)

(defpackage #:db-test
  (:use #:cl
        #:cl-async
        #:blackbird
        #:prove
        #:lparallel
        #:twitag/db)
  (:shadowing-import-from #:prove #:*debug-on-error*)
  (:shadowing-import-from #:cl-async #:delay)
  (:shadowing-import-from #:blackbird #:chain #:promise))

(in-package #:db-test)


(defparameter *test-db-file* "./test.db")

(plan 11)

(if (probe-file *test-db-file*)
  (delete-file *test-db-file*))
(defparameter *test-db* (init-db "./test.db" 1))

(with-event-loop (:catch-app-errors t)
  (alet ((count (add-user-tag 1 "test")))
    (is count 1 "add-user-tag - new record"))

  (alet ((count (add-user-tag 1 "測試")))
    (is count 1 "add-user-tag - cjk characters"))

  (alet ((count (add-user-tag 1 "測試")))
    (is count 2 "add-user-tag - updating counter"))

  (add-user-tag 1 "テスト")
  (add-user-tag 1 "テスト")
  (alet ((count (add-user-tag 1 "テスト")))
    (is count 3 "add-user-tag - more counter updates"))

  (alet ((count (add-user-tag 2 "test")))
    (is count 1 "add-user-tag - alternative user id"))

  (alet ((tags (get-user-tags 1)))
    (is tags '(("テスト" 3) ("測試" 2) ("test" 1)) :test #'equal "get-user-tags"))

  (alet ((users (get-tagged-users "test")))
    (is users '(2 1) :test #'equal "get-tagged-users"))

  (alet ((users (get-tagged-users "測試")))
    (is users '(1) :test #'equal "get-tagged-users - alternative tag"))
  
  (alet ((res (remove-user-tag 1 "測試")))
    (declare (ignore res))
    (alet ((tags (get-user-tags 1)))
      (is tags '(("テスト" 3) ("test" 1)) :test #'equal "remove-user-tag"))
    
    ;; Nested ALET expressions need careful manipulation
    ;; to ensure the proper execution order.
    (alet ((res (remove-all-user-tags 1)))
      (declare (ignore res))
      (alet ((tags (get-user-tags 1)))
        (is tags '() :test #'equal "remove-all-user-tags"))
      (alet ((tags (get-user-tags 2)))
        (is tags '(("test" 1)) :test #'equal "remove-all-user-tags - other user tags")))))

(close-db *test-db*)

(finalize)
