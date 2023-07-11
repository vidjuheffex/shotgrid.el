;; -*- lexical-binding: t; -*-

(defun shotgrid--get-current-user (callback token)
  (let ((request-url (concat shotgrid-url "api/v1/entity/HumanUser/_search"))
        (request-data (json-encode `(("filters" . [[ "email" "is" ,shotgrid-username]])))))
    (request
     request-url
     :type "POST"
     :headers `(("Authorization" . ,(format "Bearer %s" token))
                ("Accept" . "application/json")
                ("Content-Type" . "application/vnd+shotgun.api3_array+json"))
     :data request-data
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (funcall callback data))))))


(defun shotgrid--get-access-token (callback &optional refresh-token)
  "Get the access token from the Shotgrid REST API"
  (let ((credentials (auth-source-search :host shotgrid-auth-source-service
                                         :require '(:user :secret))))
    (when credentials
      (let ((entry (car credentials)))
        (setq shotgrid-username (plist-get entry :user))
        (setq shotgrid-password (let ((secret (plist-get entry :secret)))
                                  (if (functionp secret)
                                      (funcall secret)
                                    secret)))
        (when (and shotgrid-username shotgrid-password)
          (let* ((is-refresh (and refresh-token (not (null refresh-token))))
                 (grant-type (if is-refresh "refresh_token" "password"))
                 (data (if is-refresh
                           (format "refresh_token=%s&grant_type=%s" refresh-token grant-type)
                         (format "username=%s&password=%s&grant_type=%s" shotgrid-username shotgrid-password grant-type))))
            (request (concat shotgrid-url "api/v1/auth/access_token")
              :type "POST"
              :headers '(("Content-Type" . "application/x-www-form-urlencoded")
                         ("Accept" . "application/json"))
              :data data
              :parser 'json-read
              :success (cl-function
                        (lambda (&key data &allow-other-keys)
                          (funcall callback
                                   (cons (alist-get 'access_token data) (+ (time-to-seconds) (alist-get 'expires_in data)))
                                   (cons (alist-get 'refresh_token data) (+ (time-to-seconds) 86400))))))))))))

(provide 'shotgrid-auth)
