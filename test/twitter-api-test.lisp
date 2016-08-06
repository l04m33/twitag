(in-package #:cl-user)

(defpackage #:twitter-api-test
  (:use #:cl
        #:prove
        #:twitag/twitter-api
        #:cl-json
        #:babel))

(in-package #:twitter-api-test)


(plan 17)

(let ((dummy-verifier (format nil "~7,'0d" (random 10000000))))
  (is
    (let* ((dummy-session (make-twitter-session "dummy" "dummy"))
           (verifier-cb (cli-oob-verifier-cb dummy-session)))
      (with-input-from-string (*standard-input* dummy-verifier)
        (funcall verifier-cb)))
    dummy-verifier
    :test #'equal
    "cli-oob-verifier-cb"))

(is
  (multiple-value-list (access-json (decode-json-from-string "{}") :dummy))
  '(nil nil)
  "access-json - empty object")

(let ((dummy-json
        (decode-json-from-string
          "{\"a\": 1, \"b_hyphenated_val\": 2, \"nested\": {\"c\": \"nested value\"}, \"empty_array\": []}")))
  (is
    (multiple-value-list (access-json dummy-json :a))
    '(1 t)
    "access-json - top-level value")

  (is
    (multiple-value-list (access-json dummy-json :b--hyphenated--val))
    '(2 t)
    "access-json - top-level value with hyphens")

  (is
    (multiple-value-list (access-json dummy-json :nested :c))
    '("nested value" t)
    :test #'equal
    "access-json - nested value")

  (is
    (multiple-value-list (access-json dummy-json :does :not :exist))
    '(nil nil)
    "access-json - non-existent value")

  (is
    (multiple-value-list (access-json dummy-json :empty--array))
    '(nil t)
    "access-json - empty array"))

(ok
  (with-input-from-string (input (format nil "~c~c" #\return #\linefeed))
    (let ((line (read-line input)))
      (twitag/twitter-api::keep-alive-nl-p line)))
  "keep-alive-nl-p - positive")

(ok
  (not
    (with-input-from-string (input (format nil "dummy line~c~c" #\return #\linefeed))
      (let ((line (read-line input)))
        (twitag/twitter-api::keep-alive-nl-p line))))
  "keep-alive-nl-p - negative")

(ok
  (with-input-from-string (input (format nil "dummy line~c~c" #\return #\linefeed))
    (let ((line (read-line input)))
      (twitag/twitter-api::full-line-p line)))
  "full-line-p - positive")

(ok
  (not
    (with-input-from-string (input "dummy partial line")
      (let ((line (read-line input)))
        (twitag/twitter-api::full-line-p line))))
  "full-line-p - negative")

(ok
  (with-input-from-string (input (format nil "~c~c" #\return #\linefeed))
    (let ((line (read-line input)))
      (twitag/twitter-api::full-line-p line)))
  "full-line-p - empty line")

(ok
  (not
    (with-input-from-string (input (format nil "~c" #\linefeed))
      (let ((line (read-line input)))
        (twitag/twitter-api::full-line-p line))))
  "full-line-p - empty string")

(let* (results
       (message-cb #'(lambda (m) (push m results)))
       (parser (twitag/twitter-api::make-streaming-body-parser message-cb))
       (new-line (format nil "~c~c" #\return #\linefeed)))

  (funcall parser (string-to-octets new-line) 0 2)
  (is
    results '(:keep-alive)
    "streaming-body-parser - keep-alive")

  (setf results nil)
  (let ((chunk (string-to-octets "{\"a\": 1, ")))
    (funcall parser chunk 0 (length chunk)))
  (is
    results '(:partial-line)
    "streaming-body-parser - partial-line")

  (setf results nil)
  (let ((chunk (string-to-octets (format nil "\"b\": 2}~a" new-line))))
    (funcall parser chunk 0 (length chunk)))
  (is
    results '(((:a . 1) (:b . 2)))
    :test #'equal
    "streaming-body-parser - json message")

  (setf results nil)
  (let ((chunk (string-to-octets
                 (format nil "~a{\"a\": 1}~a~a~a{"
                         new-line new-line new-line new-line))))
    (funcall parser chunk 0 (length chunk)))
  (is
    (reverse results) '(:keep-alive ((:a . 1)) :keep-alive :keep-alive :partial-line)
    :test #'equal
    "streaming-body-parser - multiple messages"))

(finalize)
