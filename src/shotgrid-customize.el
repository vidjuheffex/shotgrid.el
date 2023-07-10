(defcustom shotgrid-url "https://mycompany.shotgridstudio.com/"
  "The URL of the Shotgrid site."
  :type 'string
  :group 'shotgrid)

(defcustom shotgrid-auth-source-service "shotgrid"
  "The service identifier to use when looking up Shotgrid credentials with auth-source."
  :type 'string
  :group 'shotgrid)

(provide 'shotgrid-customize)
