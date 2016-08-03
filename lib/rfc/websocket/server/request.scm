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

;; vi:se expandtab:
