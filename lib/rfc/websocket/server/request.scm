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
  (let [[ header-Upgrade (rfc822-header-ref rfc822 "Upgrade") ]
        [ header-Connection (rfc822-header-ref rfc822 "Connection") ]
        [ header-Sec-WebSocket-Key (rfc822-header-ref rfc822 "Sec-WebSocket-Key") ]
        [ header-Sec-WebSocket-Version (rfc822-header-ref rfc822 "Sec-WebSocket-Version") ]
        ]
    (errorf "not implemented")
    ) )

;; vi:se expandtab:
