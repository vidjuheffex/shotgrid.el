;; -*- lexical-binding: t; -*-

(defun shotgrid--create-api-handler ()
  "Create a handler for the Shotgrid REST API. Token and refresh token are stored as pairs."
  ;; Token Value . Expiry Date
  ;; (ey254347sv34 . 6066623)
  ;; (bh234gf23g34 . 6069623)
  (let ((token-pair nil)
        (refresh-token-pair nil))
    (lambda (callback)
      (cond
       ;; if access token is valid, use it
       ((and token-pair (< (time-to-seconds) (cdr token-pair)))
        (funcall callback (car token-pair)))
       ;; if refresh token is valid, use it to get new access token
       ((and refresh-token-pair (< (time-to-seconds) (cdr refresh-token-pair)))
        (shotgrid--get-access-token
         (lambda (new-token-pair new-refresh-token-pair)
           (setq token-pair new-token-pair)
           (setq refresh-token-pair new-refresh-token-pair)
           (funcall callback (car token)))
         (car refresh-token)))
       ;; if neither token is valid, get new access token with password grant
       (t
        (shotgrid--get-access-token
         (lambda (new-token-pair new-refresh-token-pair)
           (setq token-pair new-token-pair)
           (setq refresh-token-pair new-refresh-token-pair)
           (funcall callback (car token-pair)))))))))

(setq shotgrid--use-api (shotgrid--create-api-handler))

(provide 'shotgrid-api)                 
