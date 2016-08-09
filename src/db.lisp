(in-package #:twitag/db)


(defvar *db*)


(defun init-db (db-file worker-count)
  (vom:debug "Connecting to DB file: ~s" db-file)
  (let ((*db* (connect db-file)))
    (create-tables)
    (setf *kernel*
          (make-kernel
            worker-count
            :bindings `((*standard-output* . ,*standard-output*)
                        (*error-output* . ,*error-output*)
                        (*db* . ,*db*))))
    *db*))


(defun close-db (db)
  (vom:debug "Closing DB: ~s" db)
  (end-kernel :wait t)
  (disconnect db))


(defmacro with-db ((db-file worker-count) &body body)
  (let ((db-handle (gensym)))
    `(let ((,db-handle (init-db ,db-file ,worker-count)))
       (unwind-protect
         (progn ,@body)
         (close-db ,db-handle)))))


(defun create-tables ()
  (execute-non-query
    *db*
    "CREATE TABLE IF NOT EXISTS user_tag
     (user_id INTEGER,
      tag     TEXT,
      count   INTEGER,
      updated TEXT,
      UNIQUE(user_id, tag));"))


(defun insert-user-tag (user-id tag notifier)
  (execute-non-query
    *db* "INSERT OR REPLACE INTO user_tag (user_id, tag, count, updated)
          VALUES (?, ?,
                  COALESCE((SELECT count + 1 FROM user_tag WHERE user_id = ? and tag = ?), 1),
                  STRFTIME('%Y-%m-%d %H:%M:%f', 'now'));"
    user-id tag user-id tag)
  (let ((new-count (execute-single
                     *db* "SELECT count FROM user_tag WHERE user_id = ? AND tag = ?;"
                     user-id tag)))
    (trigger-notifier notifier)
    new-count))


(defun add-user-tag (user-id tag)
  (with-promise (resolve reject)
    (let* ((chan (make-channel))
           (notifier (make-notifier
                       #'(lambda ()
                           (let ((count (receive-result chan)))
                             (vom:debug "DB operation (~s ~s) completed, count = ~s"
                                        user-id tag count)
                             (resolve count))))))
      (submit-task chan #'insert-user-tag user-id tag notifier))))


(defun select-user-tags (user-id notifier)
  (let ((tags (execute-to-list
                *db* "SELECT tag, count FROM user_tag WHERE user_id = ? ORDER BY count DESC, updated DESC;"
                user-id)))
    (trigger-notifier notifier)
    tags))


(defun get-user-tags (user-id)
  (with-promise (resolve reject)
    (let* ((chan (make-channel))
           (notifier (make-notifier
                       #'(lambda ()
                           (let ((tags (receive-result chan)))
                             (vom:debug "Tags for user ~s: ~s" user-id tags)
                             (resolve tags))))))
      (submit-task chan #'select-user-tags user-id notifier))))


(defun delete-user-tag (user-id tag notifier)
  (execute-non-query
    *db* "DELETE FROM user_tag WHERE user_id = ? AND tag = ?;"
    user-id tag)
  (trigger-notifier notifier))


(defun remove-user-tag (user-id tag)
  (with-promise (resolve reject)
    (let* ((chan (make-channel))
           (notifier (make-notifier
                       #'(lambda ()
                           (receive-result chan)
                           (vom:debug "Removed tag ~s for user ~s" tag user-id)
                           (resolve nil)))))
      (submit-task chan #'delete-user-tag user-id tag notifier))))


(defun delete-all-user-tags (user-id notifier)
  (execute-non-query
    *db* "DELETE FROM user_tag WHERE user_id = ?;"
    user-id)
  (trigger-notifier notifier))


(defun remove-all-user-tags (user-id)
  (with-promise (resolve reject)
    (let* ((chan (make-channel))
           (notifier (make-notifier
                       #'(lambda ()
                           (receive-result chan)
                           (vom:debug "Removed all tags for user ~s" user-id)
                           (resolve nil)))))
      (submit-task chan #'delete-all-user-tags user-id notifier))))
