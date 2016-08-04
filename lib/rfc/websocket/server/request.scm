(define-module rfc.websocket.server.request
  (use rfc.822)
  (use rfc.websocket.state :prefix state:)
  (export
    <websocket-server-request>
    ) )

(select-module rfc.websocket.server.request)

(define-class <websocket-server-request> ()
  [[ state :init-value state:connecting :accessor request-state ]
   [ header :init-keyword :header :init-form (errorf ":header missing") ]
   ])

(define (request<-rfc822 header)
  (make <websocket-server-request> :header header) )

(define rfc822->request request<-rfc822)

;; vi:se expandtab:
