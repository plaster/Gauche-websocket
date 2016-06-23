(define-module rfc.websocket
  (use gauche.uvector)
  )

;;;; protocol specification: RFC 6455
;;;; https://tools.ietf.org/html/rfc6455

(define (%%mapper<-alist ks vs else-expr)
  `(lambda (<>)
     (case <>
       ,@(map (lambda (k v)
		`((,k) ,v)
		)
	      (eval ks (current-module))
	      (eval vs (current-module)))
       (else ,else-expr)
       )))

(define-macro %mapper<-alist %%mapper<-alist)

(define %table-opcode-symbol
  '(( #x0 . continue )
    ( #x1 . text )
    ( #x2 . binary )
    ;; %x3-7 are reserved for further non-control frames
    ( #x8 . connection )
    ( #x9 . ping )
    ( #xA . pong )
    ;; %xB-F are reserved for further control frames
    ))

(define opcode<-symbol
  (%mapper<-alist (map cdr %table-opcode-symbol)
		  (map car %table-opcode-symbol)
		  (errorf "unknown symbol: ~s" <>)))

(define symbol<-opcode
  (%mapper<-alist (map car %table-opcode-symbol)
		  (map (lambda (p) `',(cdr p)) %table-opcode-symbol)
		  #f))

(define opcode-symbol?
  (%mapper<-alist (map cdr %table-opcode-symbol)
		  (map (lambda (_) #t) %table-opcode-symbol)
		  #f))

(define opcode?
  (%mapper<-alist (map car %table-opcode-symbol)
		  (map (lambda (_) #t) %table-opcode-symbol)
		  #f))

(define opcode-continue      (opcode<-symbol 'continue      ))
(define opcode-text          (opcode<-symbol 'text          ))
(define opcode-binary        (opcode<-symbol 'binary        ))
(define opcode-connection    (opcode<-symbol 'connection    ))
(define opcode-ping          (opcode<-symbol 'ping          ))
(define opcode-pong          (opcode<-symbol 'pong          ))


(define (build-frame
	  :key
	  [ fin? #f ]
	  [ opcode (error "opcode required") ]
	  )
  )
