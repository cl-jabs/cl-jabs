(in-package #:swank@generic@plugin@jabs)

(defvar *swank-port* 4005)
(defvar *swank-host* "127.0.0.1")

(defbout :swank
  :validate
  :initialize
  :compile
  :swank-start-server
  )

(defbout :swank-dev
  :validate
  :initialize
  :compile
  :test
  :swank-start-server
  )

(defround :swank-start-server :swank-start-server)

;; (append-hit :compile :swank-start-server)

(defhit swank-start-server () ()
  (apply
   (tools@jabs:tosymbol "create-server" :swank)
   `(:style :spawn :dont-close t :port ,*swank-port*))
  (loop (sleep 1)))

(bind-jabs-cli-parameter
 "swank-port"
 #'(lambda (x)
     (setf *swank-port* (parse-integer x))))

(bind-jabs-cli-parameter
 "swank-host"
 #'(lambda (x)
     (setf *swank-host* x)))

(bind-project-symbol
 :swank
 #'(lambda (x)
     (let ((host (cadr (member :host x)))
	   (port (cadr (member :port x))))
       (when host
	 (setf *swank-host* host))
       (when port
	 (setf *swank-port* port)))))

(add-hook *pre-run-project-hook*
	  #'(lambda (X)
	      (eval `(setf ,(tools@jabs:tosymbol "*LOOPBACK-INTERFACE*" :swank) *swank-host*))))

(process-jabs-cli-parameter "swank-host")
(process-jabs-cli-parameter "swank-port")

