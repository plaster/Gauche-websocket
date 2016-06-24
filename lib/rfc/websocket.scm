(define-module rfc.websocket
  (use gauche.uvector)
  (use srfi-11)
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
(define (assert-opcode x) (or (opcode? x) (errorf "unknown opcode ~s" x)))

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
	  [ masking-key #f ]
	  [ extension-data (error "extension-data required") ]
	  [ application-data (error "application-data required") ]
	  [ payload-len #f ]
	  )
  (assert-opcode opcode)

  (let*-values
    [[(payload-len)
      (or payload-len
	  (+ (if extension-data (u8vector-length extension-data) 0)
	     (if application-data (u8vector-length application-data) 0)
	     )) ]
     [(payload-len-b1  payload-len-bs)
      (cond
	[(<= payload-len 125)
	 (values 125) ]
	[(<= payload-len 0xFFFF)
	 (values 126
		 (logand 0xFF (ash payload-len -8))
		 (logand 0xFF (ash payload-len 0))
		 ) ]
	[(<= payload-len 0x7FFFFFFFFFFFFFFF)
	 (values 127
		 (logand 0xFF (ash payload-len -56))
		 (logand 0xFF (ash payload-len -48))
		 (logand 0xFF (ash payload-len -40))
		 (logand 0xFF (ash payload-len -32))
		 (logand 0xFF (ash payload-len -24))
		 (logand 0xFF (ash payload-len -16))
		 (logand 0xFF (ash payload-len -8))
		 (logand 0xFF (ash payload-len 0))
		 ) ]
	) ]

     [(b0 b1)
      (values (logior (ash (if fin? 1 0) 7)
		      opcode
		      )
	      (logior (ash (if masking-key 1 0) 7)
		      payload-len-b1
		      )
	      ) ]
     ]
    ;; TODO: build
    #f

    ) )
