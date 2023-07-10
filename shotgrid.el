;;; -*- lexical-binding: t; -*-

;;; shotgrid.el --- a simple package                     

;; Copyright (C) 2023  Julian Herrera

;; Author: Julian Herrera <julian.herrera@live.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For shotgrid developers, shotgrid.el provides lighning fast access to
;; the shotgrid API and contents.

;;; Code:

(require 'request)
(require 'json)

(defun shotgrid--get-access-token (callback &optional refresh-token)
  "Get the access token from the Shotgrid REST API"
  (let ((credentials (auth-source-search :host "shotgrid"
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
           (funcall callback (car token))) (car refresh-token)))
       ;; if neither token is valid, get new access token with password grant
       (t
        (shotgrid--get-access-token
         (lambda (new-token-pair new-refresh-token-pair)
           (setq token-pair new-token-pair)
           (setq refresh-token-pair new-refresh-token-pair)
           (funcall callback (car token-pair)))))))))

(setq shotgrid--use-api (shotgrid--create-api-handler))

(cl-defun shotgrid-request (&key (type "GET") (entity nil) (callback nil) (params nil))
  "Make a request to the Shotgrid REST API"
  (funcall shotgrid--use-api
           (lambda (token)
             (let ((request-url (concat shotgrid-url "api/v1/entity/" entity)))
               (request request-url
                 :type type
                 :headers `(("Authorization" . ,(format "Bearer %s" token))
                            ("Accept" . "application/json"))
                 :params params
                 :parser 'json-read
                 :success (cl-function
                           (lambda (&key data &allow-other-keys)
                             (when callback
                               (funcall callback data))))
                 :error (cl-function
                         (lambda (&key response error-thrown symbol-status &allow-other-keys)
                           (message "An error occurred: %S" error-thrown)
                           (message "Error symbol: %S" symbol-status)
                           (message "Error data: %S" (request-response-error-thrown response))
                           (message "Attempted URL: %S" request-url)
                           (when (request-response-data response)
                             (message "Response data: %S" (request-response-data response))))))))))


(defun shotgrid--get-projects (&optional callback)
  "Get a list of projects from the Shotgrid REST API"
  (shotgrid-request :entity "projects"
                    :params '(("filter[sg_status]" . "in production")
                              ("fields" . "name,id,sg_status"))
                    :callback (lambda (data)
                                (let ((projects (mapcar (lambda (project)
                                                          (cons (cdr (assoc 'name (assoc 'attributes project)))
                                                                (cdr (assoc 'id project))))
                                                        (alist-get 'data data))))
                                  (when callback
                                    (funcall callback projects))))))

(defun shotgrid--open-project-in-browser (project-id)
  "Open the selected project in the browser."
  (browse-url (concat shotgrid-url "page/project_overview?project_id=" (number-to-string project-id))))


(defun shotgrid--pick-project-action (project-id)
  "Pick what do do with the selected project."
  (let ((key (char-to-string (read-key "[o] Open Project in Browser"))))
    (cond ((equal key "o") (shotgrid--open-project-in-browser project-id))
          (t (message "Invalid key")))))

(defun shotgrid--pick-project (projects)
  "List all projects in shotgrid."
  (let ((choice (completing-read "Select a project: " projects)))
    (shotgrid--pick-project-action (cdr (assoc choice projects)))))

(defun shotgrid-list-projects ()
  "List all projects in shotgrid."
  (interactive)
  (shotgrid--get-projects 'shotgrid--pick-project))

(defun shotgrid-list-favorites ()
  "List all favorite projects in shotgrid."
  (interactive)
  (shotgrid--get-favorites 'shotgrid--pick-favorite))

(defun shotgrid--pick-favorite (favorites)
  "List all favorite projects in shotgrid."
  (let ((choice (completing-read "Select a page: " favorites)))
    (message "hello")
    nil))

(defun shotgrid--get-favorites (&optional callback)
  "Get a list of favorite projects from the Shotgrid REST API"
  (shotgrid-request :entity "HumanUser"
                    :params `(("filter[email]" . ,shotgrid-username)
                              ("fields" . "email"))
                    :callback (lambda (data)
                                (message "%S" data)
                                (let ((favorites (mapcar (lambda (favorite)
                                                          (cdr (assoc 'email (assoc 'attributes favorite))))
                                                        (alist-get 'data data))))
                                  (when callback
                                    (funcall callback favorites))))))
  


(provide 'shotgrid)
;;; shotgrid.el ends here

