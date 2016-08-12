(in-package #:cl-user)

(defpackage #:twitag-test
  (:use #:cl
        #:cl-async
        #:blackbird
        #:prove
        #:twitag)
  (:shadowing-import-from #:blackbird #:*debug-on-error*))

(in-package #:twitag-test)

(plan 9)

(is
  (twitag::build-tags-str '((:id . 1234)
                            (:screen-name . "dummy")
                            (:tags . (((:text . "dummy_tag")
                                       (:count . 2))
                                      ((:text . "dummy_tag_2")
                                       (:count . 1))))))
  "@dummy : #dummy_tag #dummy_tag_2"
  :test #'equal
  "build-tags-str")

(is
  (twitag::build-tags-str '((:id . 1234)
                            (:screen-name . "dummy")
                            (:tags . ())))
  "似乎沒人關心 @dummy Q_Q"
  :test #'equal
  "build-tags-str - no tags")

(let (reply)
  (labels ((body-text-builder (text &key cur-user-id broadcast)
             (declare (ignore cur-user-id broadcast))
             text)
           (build-users-str (user-list)
             (twitag::build-users-str user-list :screen-name-prefix ""))
           (replier (text)
             (setf reply text))
           (blocking-p (user-id)
             (declare (ignore user-id))
             nil))

    (with-event-loop (:catch-app-errors t)
      (twitag::process-result
        nil
        #'body-text-builder
        #'build-users-str
        #'replier
        #'blocking-p))
    (is reply nil
        "process-result - empty result")

    (setf reply nil)
    (with-event-loop (:catch-app-errors t)
      (twitag::process-result
        '(:add-user-tags
           ((:id . 1234)
            (:screen-name . "dummy")
            (:tags ((:text . "tag1") (:count 2))
                   ((:text . "tag2") (:count 1))))
           ((:id . 2345)
            (:screen-name . "dummy2")
            (:tags ((:text . "tag3") (:count 2))
                   ((:text . "tag4") (:count 1)))))
        #'body-text-builder
        #'build-users-str
        #'replier
        #'blocking-p))
    (is reply "好的，我記在小本子上了~"
        "process-result - add-user-tags")

    (setf reply nil)
    (with-event-loop (:catch-app-errors t)
      (twitag::process-result
        '(:get-user-tags
           ((:id . 1234)
            (:screen-name . "dummy")
            (:tags ((:text . "tag1") (:count 2))
                   ((:text . "tag2") (:count 1)))))
        #'body-text-builder
        #'build-users-str
        #'replier
        #'blocking-p))
    (is reply "@dummy : #tag1 #tag2"
        "process-result - get-user-tags")

    (setf reply nil)
    (with-event-loop (:catch-app-errors t)
      (twitag::process-result
        '(:get-user-tags
           ((:id . 1234)
            (:screen-name . "dummy")
            (:tags)))
        #'body-text-builder
        #'build-users-str
        #'replier
        #'blocking-p))
    (is reply "似乎沒人關心 @dummy Q_Q"
        "process-result - empty tags")

    (setf reply nil)
    (with-event-loop (:catch-app-errors t)
      (twitag::process-result
        '(:get-tagged-users
           ((:screen--name . "dummy1"))
           ((:screen--name . "dummy2"))
           ((:screen--name . "dummy3"))
           ((:screen--name . "dummy4")))
        #'body-text-builder
        #'build-users-str
        #'replier
        #'blocking-p))
    (is reply "dummy1 dummy2 dummy3 dummy4"
        "process-result - get-tagged-users")

    (setf reply nil)
    (with-event-loop (:catch-app-errors t)
      (twitag::process-result
        '(:get-tagged-users)
        #'body-text-builder
        #'build-users-str
        #'replier
        #'blocking-p))
    (is reply "沒有這樣的人哦~"
        "process-result - empty users")

    (setf reply nil)
    (labels ((blocking-p (user-id) (declare (ignore user-id)) t))
      (with-event-loop (:catch-app-errors t)
        (twitag::process-result
          '(:get-user-tags
             ((:id . 1234)
              (:screen-name . "dummy")
              (:tags ((:text . "tag1") (:count 2))
                     ((:text . "tag2") (:count 1)))))
          #'body-text-builder
          #'build-users-str
          #'replier
          #'blocking-p)))
    (is reply "@dummy 遮住了小本子不讓大家看~"
        "process-result - blocked")))

(finalize)
