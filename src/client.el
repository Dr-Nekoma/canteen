(setq *mailbox* nil)

(defun perplex-handler (process response)
  ""
  ;; (setq *mailbox* (json-read-from-string response))
  (setq *mailbox* response)
  (let ((response (process-get process :response)))
    (message "Received: %s" response)    
    (delete-process process)))

(setq perplex-address "localhost")
(setq perplex-port 7778)

(defun perplex-client (content)
  "" 
  (let ((connection (open-network-stream "perplexdb" "*perplex-tcp*" perplex-address perplex-port)))
    (process-put connection :response nil)
    (set-process-filter connection 'perplex-handler)
    (process-send-string connection content)))

(perplex-client (concat "WAbc" (char-to-string #x2) "trashy" (char-to-string ?\C-d)))
(print *mailbox*)
