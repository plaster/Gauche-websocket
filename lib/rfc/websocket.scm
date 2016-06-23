(define-module rfc.websocket
  (use gauche.uvector)
  )

(define opcode-continue      #x0 )
(define opcode-text          #x1 )
(define opcode-binary        #x2 )
(define opcode-connection    #x8 )
(define opcode-ping          #x9 )
(define opcode-pong          #xA )

(define (symbol<-opcode opcode)
  (case opcode
    (( #x0 ) 'continue           )
    (( #x1 ) 'text               )
    (( #x2 ) 'binary             )
    (( #x8 ) 'connection         )
    (( #x9 ) 'ping               )
    (( #xA ) 'pong               )
    (else #f )
    ))

(define (opcode<-symbol s)
  (case s
    (( continue   )          #x0 )
    (( text       )          #x1 )
    (( binary     )          #x2 )
    (( connection )          #x8 )
    (( ping       )          #x9 )
    (( pong       )          #xA )
    (else (errorf "unknown symbol: ~s" s) )
    ))

(define (build-frame
	  :key
	  [ fin? #f ]
	  [ opcode (error "opcode required") ]
	  )
  )
