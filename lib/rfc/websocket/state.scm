(define-module rfc.websocket.state
  (export
    connecting open closing closed))

(select-module rfc.websocket.state)

(define-constant connecting 'connecting)
(define-constant open 'open)
(define-constant closing 'closing)
(define-constant closed 'closed)

;; vi:se expandtab:
