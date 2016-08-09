(in-package #:twitag)


(defmacro def-msg (name (&rest args) text)
  `(defmacro ,name ,args
     `(funcall #'format nil ,,text ,,@args)))

(def-msg msg-empty-tags (screen-name)
  "似乎沒人關心 @~a Q_Q")
(def-msg msg-error ()
  "我~~出~~錯~~啦~~讓~~我~~一~~個~~人~~靜~~靜~~")
(def-msg msg-tag-added ()
  "好的，我記在小本子上了~~")
(def-msg msg-user-blocking (screen-name)
  "@~a 遮住了小本子不讓大家看~~")


(defun catched-call (func &rest args)
  (catcher (apply func args)
           (t (e)
              (vom:error "~s" e)
              nil)))


(defun trim-twitter-text (text)
  (if (> (length text) 140)
    (concatenate 'string (subseq text 0 136) "....")
    text))


(defun trim-tags-str (text)
  (labels ((find-last-space (text)
             (loop for c across text
                   for i from 0
                   with last-space = -1
                   do (cond
                        ((>= i 136)
                         (return-from find-last-space last-space))
                        ((eql #\space (aref text i))
                         (setf last-space i)))
                   finally (return last-space))))
    (if (> (length text) 140)
      (let ((last-space (find-last-space text)))
        (if (>= last-space 0)
          (concatenate 'string (subseq text 0 last-space) " ....")
          (trim-twitter-text text)))
      text)))


(defun filter-self-mention (my-id mentions)
  (loop for m in mentions
        when (/= my-id (access-json m :id))
        collect m))


(defun rt-text-p (mentions text)
  (labels ((look-back-for-rt (text idx)
             (if (and (>= idx 0) (eql (aref text idx) #\space))
               (look-back-for-rt text (1- idx))
               (if (> idx 0)
                 (or (and (eql #\R (aref text (1- idx))) (eql #\T (aref text idx)))
                     (and (eql #\r (aref text (1- idx))) (eql #\t (aref text idx))))
                 nil)))
           (mention-rt-p (mentions)
             (if mentions
               (let* ((m (car mentions))
                      (m-range (access-json m :indices))
                      (m-start (nth 0 m-range)))
                 (or (look-back-for-rt text (1- m-start))
                     (mention-rt-p (cdr mentions))))
               nil)))
    (mention-rt-p mentions)))


(defun rt-p (message)
  (or (access-json message :retweeted--status)
      (rt-text-p (access-json message :entities :user--mentions)
                 (access-json message :text))))


(defun mentioned-p (user-id mentions)
  (dolist (m mentions nil)
    (when (= user-id (access-json m :id))
      (return-from mentioned-p m))))


(defun self-status-p (my-id message)
  (let ((user-id (access-json message :user :id)))
    (= user-id my-id)))


(defun build-tags-str (user-obj)
  (if (access-json user-obj :tags)
    (apply #'concatenate 'string
           (format nil "@~a :" (access-json user-obj :screen-name))
           (mapcar #'(lambda (tag)
                       (concatenate 'string " #" (access-json tag :text)))
                   (access-json user-obj :tags)))
    (msg-empty-tags (access-json user-obj :screen-name))))


(defun build-status-reply (user mentions text)
  (with-output-to-string (output)
    (when user
      (format output "@~a " (access-json user :screen--name)))
    (loop for m in mentions
          with user-list = (if user
                             `(,(access-json user :screen--name))
                             nil)
          do (let ((cur-screen-name (access-json m :screen--name)))
               (when (not (find cur-screen-name user-list :test #'equal))
                 (push cur-screen-name user-list)
                 (format output "@~a " cur-screen-name))))
    (format output "~a" text)))


(defun handle-status-message (message my-id session)
  (let ((all-mentions (access-json message :entities :user--mentions)))
    (when (and (not (self-status-p my-id message))
               (mentioned-p my-id all-mentions)
               (not (rt-p message)))
      (alet* ((reply-user (access-json message :user))
              (reply-status-id (access-json message :id))
              (mentions (filter-self-mention my-id all-mentions))
              (hashtags (access-json message :entities :hashtags))
              (result (catcher
                        (handle-mentions-and-hashtags mentions hashtags)
                        (t (e) '(:error e)))))
        (labels ((body-text-builder (text &key cur-user-id broadcast)
                   (let ((bc-mentions (if broadcast mentions nil))
                         (u (if (and cur-user-id
                                     (= cur-user-id
                                        (access-json reply-user :id)))
                              nil
                              reply-user)))
                     (trim-tags-str (build-status-reply u bc-mentions text))))
                 (replier (text)
                   (statuses-update session text reply-status-id))
                 (local-blocking-p (user-id)
                   (blocking-p session user-id #'body-text-builder #'replier)))
          (process-result result #'body-text-builder #'replier #'local-blocking-p))))))


(defun dm-to-self-p (my-id message)
  (let ((target-id (access-json
                     message
                     :direct--message :recipient :id)))
    (= target-id my-id)))


(defun handle-direct-message (message my-id session)
  (let ((all-mentions (access-json
                        message
                        :direct--message :entities :user--mentions)))
    (when (dm-to-self-p my-id message)
      (alet* ((reply-id (access-json message :direct--message :sender :id))
              (mentions (filter-self-mention my-id all-mentions))
              (hashtags (access-json
                          message :direct--message :entities :hashtags))
              (result (catcher
                        (handle-mentions-and-hashtags mentions hashtags)
                        (t (e) '(:error e)))))
        (labels ((body-text-builder (text &key cur-user-id broadcast)
                   (declare (ignore cur-user-id broadcast))
                   (trim-tags-str text))
                 (replier (text)
                   (direct-messages-new session reply-id text))
                 (local-blocking-p (user-id)
                   (blocking-p session user-id #'body-text-builder #'replier)))
          (process-result result #'body-text-builder #'replier #'local-blocking-p))))))


(defun blocking-p (session user-id body-text-builder replier)
  (catcher
    (user-blocking-p session user-id)
    (t (e)
       (vom:debug "user-blocking-p: error: ~s" e)
       (funcall replier
                (funcall body-text-builder (msg-error) :broadcast t))
       (error e))))


(defun process-result (result body-text-builder replier blocking-p)
  (cond
    ((null result)
     (vom:debug "process-result: No result"))

    ((eql :add-user-tags (car result))
     (vom:debug "process-result: Added user tag: ~s" (cdr result))
     (funcall replier
              (funcall body-text-builder (msg-tag-added) :broadcast t)))

    ((eql :get-user-tags (car result))
     (vom:debug "process-result: Tags for user: ~s" (cdr result))
     (dolist (u (cdr result))
       (alet* ((user-id (access-json u :id))
               (user-screen-name (access-json u :screen-name))
               (blocking (funcall blocking-p user-id)))
         (vom:debug "blocking = ~s" blocking)
         (if blocking
           (funcall replier
                    (funcall body-text-builder
                             (msg-user-blocking user-screen-name)
                             :broadcast nil))
           (let* ((tags-str (build-tags-str u))
                  (reply-str (funcall body-text-builder
                                      tags-str
                                      :cur-user-id user-id
                                      :broadcast nil)))
             (funcall replier reply-str))))))

    ((eql :error (car result))
     (vom:debug "process-result: error: ~s" (cadr result)))))


(defun handle-mentions-and-hashtags (mentions hashtags)
  (with-promise (resolve reject)
    (cond
      ((and mentions hashtags)
       (let ((user-tags nil))
         (catcher
           (wait
             (adolist (m mentions)
               (alet* ((user-id (access-json m :id))
                       (user-screen-name (access-json m :screen--name))
                       (tag-list
                         (amap
                           #'(lambda (h)
                               (alet* ((tag-text (access-json h :text))
                                       (tag-count (add-user-tag user-id tag-text)))
                                 `((:text . ,tag-text) (:count . ,tag-count))))
                           hashtags)))
                 (push `((:id . ,user-id)
                         (:screen-name . ,user-screen-name)
                         (:tags . ,tag-list))
                       user-tags)))
             (resolve `(:add-user-tags . ,(nreverse user-tags))))
           (t (e) (reject e)))))

      (mentions
       (catcher
         (alet ((user-tags
                  (amap
                    #'(lambda (m)
                        (alet* ((user-id (access-json m :id))
                                (user-screen-name (access-json m :screen--name))
                                (tag-list (get-user-tags user-id))
                                (norm-tag-list
                                  (mapcar #'(lambda (tag)
                                              `((:text . ,(nth 0 tag))
                                                (:count . ,(nth 1 tag))))
                                          tag-list)))
                          `((:id . ,user-id)
                            (:screen-name . ,user-screen-name)
                            (:tags . ,norm-tag-list))))
                    mentions)))
           (resolve `(:get-user-tags . ,user-tags)))
         (t (e) (reject e))))

      (t
       (resolve nil)))))


(defun handle-follow-event (message state)
  (let ((source (access-json message :source))
        (target (access-json message :target))
        (my-id (gethash :user-id state))
        (friends (gethash :friends state))
        (session (gethash :session state)))
    (cond
      ((and (= my-id (access-json target :id))
            (not (member (access-json source :id) friends)))
       (alet ((result (friendships-create session (access-json source :id))))
         (if result
           (vom:debug "Followed user (~s ~s)"
                      (access-json source :id)
                      (access-json source :screen--name))
           (vom:warn "Failed to follow back user (~s ~s)"
                     (access-json source :id)
                     (access-json source :screen--name)))))
      ((= my-id (access-json source :id))
       (let ((new-friends (cons (access-json target :id) friends)))
         (vom:debug "friends: ~s" (length new-friends))
         (setf (gethash :friends state) new-friends))))))


(defun handle-unfollow-event (message state)
  (let ((source (access-json message :source))
        (my-id (gethash :user-id state)))
    (when (= my-id (access-json source :id))
      (let* ((target (access-json message :target))
             (target-id (access-json target :id))
             (friends (gethash :friends state))
             (new-friends (remove-if #'(lambda (x) (= x target-id)) friends)))
        (vom:debug "friends: ~s" (length new-friends))
        (setf (gethash :friends state) new-friends)))))


(defun message-cb (message state)
  (cond
    ((keywordp message)
     (vom:debug "message: ~s" message))

    ((nth-value 1 (access-json message :friends))
     (let ((friends (access-json message :friends)))
       (vom:debug "message: friends: ~s" (length friends))
       (setf (gethash :friends state) friends)))

    ((access-json message :text)
     (vom:debug "message: status: id = ~s" (access-json message :id))
     (vom:debug "message: status: retweeted_status = ~s"
                (not (not (access-json message :retweeted--status))))
     (vom:debug "message: status: quoted_status = ~s"
                (not (not (access-json message :quoted--status))))
     (vom:debug "message: status: mentions = ~s"
                (access-json message :entities :user--mentions))
     (vom:debug "message: status: hashtags = ~s"
                (access-json message :entities :hashtags))
     (vom:debug "message: status: text = ~s" (access-json message :text))
     (catched-call #'handle-status-message message
                   (gethash :user-id state) (gethash :session state)))

    ((access-json message :delete)
     (vom:debug "message: delete: id = ~s"
                (access-json message :delete :status :id)))

    ((access-json message :direct--message)
     (vom:debug "message: direct message: id = ~s"
                (access-json message :direct--message :id))
     (vom:debug "message: direct message: mentions = ~s"
                (access-json message :direct--message :entities :user--mentions))
     (vom:debug "message: direct message: hashtags = ~s"
                (access-json message :direct--message :entities :hashtags))
     (vom:debug "message: direct message: text = ~s"
                (access-json message :direct--message :text))
     (catched-call #'handle-direct-message message
                   (gethash :user-id state) (gethash :session state)))

    ((access-json message :event)
     (let ((event (access-json message :event)))
       (vom:debug "message: event: ~s" event)
       (cond
         ((string= event "follow")
          (catched-call #'handle-follow-event message state))
         ((string= event "unfollow")
          (handle-unfollow-event message state))
         (t
          (vom:debug "message: event: unknown: ~s" message)))))

    (t
     (vom:debug "message: unknown: ~s" message))))


 (defun make-message-cb (my-user-id my-screen-name session cb)
   (let ((state (make-hash-table)))
     (setf (gethash :user-id state) my-user-id)
     (setf (gethash :screen-name state) my-screen-name)
     (setf (gethash :session state) session)
     #'(lambda (msg) (funcall cb msg state))))


(defun install-signal-handlers ()
  (signal-handler
    +sigint+
    (lambda (sig)
      (when (= sig +sigint+)
        (vom:debug "SIGINT received. Cleaning up...")
        (free-signal-handler +sigint+)
        (exit-event-loop)))))


(defun main (consumer-key consumer-secret db-file)
  (with-db (db-file 1)
    (with-event-loop (:catch-app-errors t)
      (install-signal-handlers)
      (let ((session (make-twitter-session consumer-key consumer-secret)))
        (alet ((login-result
                 (catched-call #'login session "oob"
                               (cli-oob-verifier-cb session))))
              (vom:debug "login-result = ~s" login-result)

              (when login-result
                (let ((my-user-id (nth 0 login-result))
                      (my-screen-name (nth 1 login-result)))
                  (alet ((streaming-result
                           (catched-call
                             #'start-streaming
                             session
                             (make-message-cb my-user-id my-screen-name
                                              session #'message-cb))))
                        (vom:debug "streaming-result = ~s"
                                   streaming-result)))))))))
