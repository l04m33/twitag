(in-package #:cl-user)

(defpackage #:twitag-test
  (:use #:cl
        #:cl-async
        #:blackbird
        #:prove
        #:twitag)
  (:shadowing-import-from #:blackbird #:*debug-on-error*))

(in-package #:twitag-test)

(plan 25)

(ok
  (not
    (twitag::rt-text-p '() "dummy text"))
  "rt-text-p - empty mentions")

(ok
  (not
    (twitag::rt-text-p '(((:screen--name . "dummy")
                     (:name . "Dummy Name")
                     (:id . 1234)
                     (:id--str . "1234")
                     (:indices 0 6)))
                  "@dummy blah blah blah"))
  "rt-text-p - negative")

(ok
  (not
    (twitag::rt-text-p '(((:screen--name . "dummy")
                     (:name . "Dummy Name")
                     (:id . 1234)
                     (:id--str . "1234")
                     (:indices 10 16)))
                  "blah blah @dummy blah blah blah"))
  "rt-text-p - negative 2")

(ok
  (twitag::rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 3 9)))
                "RT @dummy blah blah blah")
  "rt-text-p - positive uppercase")

(ok
  (twitag::rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 2 9)))
                "rt @dummy blah blah blah")
  "rt-text-p - positive lowercase")

(ok
  (twitag::rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 2 9)))
                "RT@dummy blah blah blah")
  "rt-text-p - positive no-space")

(ok
  (twitag::rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 0 6))
                  ((:screen--name . "dummy2")
                   (:name . "Dummy Name 2")
                   (:id . 2345)
                   (:id--str . "2345")
                   (:indices 15 22)))
                "@dummy blah RT @dummy2 blah blah")
  "rt-text-p - positive multiple mentions")

(ok
  (twitag::rt-text-p '(((:screen--name . "dummy")
                   (:name . "Dummy Name")
                   (:id . 1234)
                   (:id--str . "1234")
                   (:indices 3 9))
                  ((:screen--name . "dummy2")
                   (:name . "Dummy Name 2")
                   (:id . 2345)
                   (:id--str . "2345")
                   (:indices 18 25)))
                "RT @dummy blah RT @dummy2 blah blah")
  "rt-text-p - positive multiple RT")

(is
  (twitag::filter-self-mention 1234 '(((:screen--name . "dummy")
                                       (:name . "Dummy Name")
                                       (:id . 1234)
                                       (:id--str . "1234")
                                       (:indices 0 6))
                                      ((:screen--name . "dummy2")
                                       (:name . "Dummy Name 2")
                                       (:id . 2345)
                                       (:id--str . "2345")
                                       (:indices 15 22))))
  '(((:screen--name . "dummy2")
     (:name . "Dummy Name 2")
     (:id . 2345)
     (:id--str . "2345")
     (:indices 15 22)))
  :test #'equal
  "filter-self-mention")

(ok
  (twitag::mentioned-p 1234 '(((:screen--name . "dummy")
                               (:name . "Dummy Name")
                               (:id . 1234)
                               (:id--str . "1234")
                               (:indices 0 6))
                              ((:screen--name . "dummy2")
                               (:name . "Dummy Name 2")
                               (:id . 2345)
                               (:id--str . "2345")
                               (:indices 15 22))))
  "mentioned-p - positive")

(ok
  (not
    (twitag::mentioned-p 1234 '(((:screen--name . "dummy2")
                                 (:name . "Dummy Name 2")
                                 (:id . 2345)
                                 (:id--str . "2345")
                                 (:indices 15 22)))))
  "mentioned-p - negative")

(ok
  (not
    (twitag::mentioned-p 1234 '()))
  "mentioned-p - empty mentions")

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

(is
  (twitag::build-status-reply nil nil "@twitter : #dummy #tags #str")
  "@twitter : #dummy #tags #str"
  :test #'equal
  "build-status-reply - null user and null mentions")

(is
  (twitag::build-status-reply
    '((:id . 1234) (:screen--name . "dummy"))
    nil "@twitter : #dummy #tags #str")
  "@dummy @twitter : #dummy #tags #str"
  :test #'equal
  "build-status-reply - null mentions")

(is
  (twitag::build-status-reply
    nil '(((:id . 1234) (:screen--name . "dummy"))
          ((:id . 2345) (:screen--name . "dummy2")))
    "dummy str")
  "@dummy @dummy2 dummy str"
  :test #'equal
  "build-status-reply - null user")

(is
  (twitag::build-status-reply
    '((:id . 1234) (:screen--name . "dummy"))
    '(((:id . 2345) (:screen--name . "dummy2"))
      ((:id . 3456) (:screen--name . "dummy3")))
    "dummy str")
  "@dummy @dummy2 @dummy3 dummy str"
  :test #'equal
  "build-status-reply")

(is
  (twitag::build-status-reply
    '((:id . 1234) (:screen--name . "dummy"))
    '(((:id . 1234) (:screen--name . "dummy"))
      ((:id . 2345) (:screen--name . "dummy2")))
    "dummy str")
  "@dummy @dummy2 dummy str"
  :test #'equal
  "build-status-reply - duplicate user")

(is
  (twitag::build-status-reply
    '((:id . 1234) (:screen--name . "dummy"))
    '(((:id . 2345) (:screen--name . "dummy2"))
      ((:id . 2345) (:screen--name . "dummy2")))
    "dummy str")
  "@dummy @dummy2 dummy str"
  :test #'equal
  "build-status-reply - duplicate mentions")

(let (reply)
  (labels ((body-text-builder (text &key cur-user-id broadcast)
             (declare (ignore cur-user-id broadcast))
             text)
           (replier (text)
             (setf reply text))
           (blocking-p (user-id)
             (declare (ignore user-id))
             nil))

    (with-event-loop (:catch-app-errors t)
      (twitag::process-result nil #'body-text-builder #'replier #'blocking-p))
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
        #'replier
        #'blocking-p))
    (is reply "似乎沒人關心 @dummy Q_Q"
        "process-result - empty tags")

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
          #'replier
          #'blocking-p)))
    (is reply "@dummy 遮住了小本子不讓大家看~"
        "process-result - blocked")))

(finalize)
