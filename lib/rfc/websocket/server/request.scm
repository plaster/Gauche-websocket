(define-module rfc.websocket.server.request
  (use rfc.822)
  (use rfc.websocket.state :prefix state:)
  (export
    <websocket-server-request>
    ) )

(select-module rfc.websocket.server.request)

(define-class <websocket-server-request> ()
  [[ state :init-value state:connecting :accessor request-state ]
   ])

(define-method handshake
  [[self <websocket-server-request>]
   rfc822
   ]
  (errorf "not implemented"))

;; vi:se expandtab:
