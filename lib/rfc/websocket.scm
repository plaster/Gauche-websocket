(define-module rfc.websocket
  (use gauche.uvector)
  )

(define opcode-continue       #x0)
(define opcode-text           #x1)
(define opcode-binary         #x2)
(define opcode-connection     #x8)
(define opcode-ping           #x9)
(define opcode-pong           #xA)

(define (build-frame
	  :key
	  [ fin? #f ]
	  [ opcode (error "opcode required") ]
	  )
  )
