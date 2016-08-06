(in-package #:cl-user)


(defpackage #:twitag/twitter-api
  (:use #:cl
        #:cl-async
        #:cl-async-oauth
        #:blackbird
        #:babel
        #:cl-json)
  (:export #:*resource-base-url*
           #:*request-token-url*
           #:*authenticate-url*
           #:*access-token-url*
           #:*user-stream-url*
           #:*friendships-create-url*
           #:*direct-messages-new-url*
           #:*statuses-user-timeline-url*
           #:make-twitter-session
           #:login
           #:cli-oob-verifier-cb
           #:resp-error
           #:access-json
           #:start-streaming
           #:friendships-create
           #:direct-messages-new
           #:user-blocking-p
           #:statuses-update))


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
           #:remove-all-user-tags))


(defpackage #:twitag
  (:use #:cl
        #:cl-async
        #:blackbird
        #:sqlite
        #:bordeaux-threads
        #:twitag/twitter-api
        #:twitag/db)
  (:export #:main))
