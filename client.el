(setq *mailbox* nil)

(defun canteen-handler (process response)
  ""
  ;; (setq *mailbox* (json-read-from-string response))
  (setq *mailbox* response)
  (let ((response (process-get process :response)))
    (message "Received: %s" response)    
    (delete-process process)))

(setq canteen-address "localhost")
(setq canteen-port 1111)

(defun canteen-client (content)
  ""
  (let ((connection (open-network-stream "canteendb" "*canteen-tcp*" canteen-address canteen-port)))
    (process-put connection :response nil)
    (set-process-filter connection 'canteen-handler)
    (process-send-string connection content)))

;; Message = "W~00030004abc0001"
(let ((message (string ?W ?~ ?\C-@ ?\C-@ ?\C-@ ?\C-c ?\C-@ ?\C-@ ?\C-@ ?\C-d ?a ?b ?c ?\C-@ ?\C-@ ?\C-@ ?\C-a)))
  (canteen-client message))

(let ((message (string ?R ?^ ?\C-@ ?\C-@ ?\C-@ ?\C-c ?a ?b ?c)))
  (canteen-client message))

(print *mailbox*)

(string 98 ?w)

(let ((message "H221F8AF2372A95064F2EF7D7712216A9AB46E7EF98482FD237E106F83EAA7569H1BC5D0E3DF0EA12C4D0078668D14924F95106BBE173E196DE50FE13A900B0937"))
  (canteen-client message))


