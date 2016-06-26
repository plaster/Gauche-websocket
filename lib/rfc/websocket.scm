(define-module rfc.websocket
  (use gauche.uvector)
  (use srfi-11)
  )

;;;; protocol specification: RFC 6455
;;;; https://tools.ietf.org/html/rfc6455

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

(let [[ opcode-list (map car %table-opcode-symbol) ]
      [ opcode-symbol-list (map cdr %table-opcode-symbol) ]
      ]
  (for-each assert-opcode opcode-list)
  (for-each (^o (or ($ = o $ opcode<-symbol $ symbol<-opcode o)
		    (errorf "opcode ~s is not invaliant on symbol mapping" o)))
	    opcode-list)
  (for-each (^s (or ($ eq? s $ symbol<-opcode $ opcode<-symbol s)
		    (errorf "symbol ~s is not invaliant on opcode mapping" s)))
	    opcode-symbol-list)
  )

(define opcode-continue      (opcode<-symbol 'continue      ))
(define opcode-text          (opcode<-symbol 'text          ))
(define opcode-binary        (opcode<-symbol 'binary        ))
(define opcode-connection    (opcode<-symbol 'connection    ))
(define opcode-ping          (opcode<-symbol 'ping          ))
(define opcode-pong          (opcode<-symbol 'pong          ))

(define (mask-payload payload-data masking-key)
  (errorf "not implemented")
  )

(define (build-frame
	  :key
	  [ fin? #f ]
	  [ opcode (error "opcode required") ]
	  [ masking-key #f ]
	  [ payload-data (error "payload-data required") ]
	  [ payload-len #f ]
	  )
  (assert-opcode opcode)

  (and masking-key
       (or (u8vector? masking-key)
	   (errorf "masking-key must be u8vector.") )
       (or ($ = 4 $ u8vector-length masking-key)
	   (errorf "masking-key length must be 4.") )
       )

  (let*-values
    [[(payload-len)
      (u8vector-length payload-data) ]
     [(payload-len-b1 payload-len-bs)
      (cond
	[(<= payload-len 125)
	 (values 125 '#u8()) ]
	[(<= payload-len #xFFFF)
	 (values 126
		 (u8vector (logand #xFF (ash payload-len -8))
			   (logand #xFF (ash payload-len 0))
			   ) ) ]
	[(<= payload-len #x7FFFFFFFFFFFFFFF)
	 (values 127
		 (u8vector (logand #xFF (ash payload-len -56))
			   (logand #xFF (ash payload-len -48))
			   (logand #xFF (ash payload-len -40))
			   (logand #xFF (ash payload-len -32))
			   (logand #xFF (ash payload-len -24))
			   (logand #xFF (ash payload-len -16))
			   (logand #xFF (ash payload-len -8))
			   (logand #xFF (ash payload-len 0))
			   ) ) ]
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
    (u8vector-append
      (u8vector b0 b1)
      payload-len-bs
      (or masking-key '#u8())
      (if masking-key
	(mask-payload payload-data masking-key)
	payload-data)
      ) ) )
