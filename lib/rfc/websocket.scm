(define-module rfc.websocket
  (use gauche.uvector)
  (use srfi-11)
  )
(select-module rfc.websocket)

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

(define (assert-masking-key masking-key)
  (or (u8vector? masking-key)
      (errorf "masking-key must be u8vector.") )
  (or ($ = 4 $ u8vector-length masking-key)
      (errorf "masking-key length must be 4.") )
  )

(define (mask-payload! payload-data masking-key)
  (dotimes [i (u8vector-length payload-data)]
    (u8vector-set! payload-data i
                   (logxor (u8vector-ref payload-data i)
                           (u8vector-ref masking-key (logand i 3)) ) ) )
  payload-data )

(define (mask-payload payload-data masking-key)
  (mask-payload! (u8vector-copy payload-data) masking-key))

(define (build-frame
          :key
          [ fin? #f ]
          [ opcode (error "opcode required") ]
          [ masking-key #f ]
          [ payload-data (error "payload-data required") ]
          )
  (assert-opcode opcode)

  (and masking-key (assert-masking-key masking-key))

  (receive (payload-len-b1 payload-len-bs)
    (let1 payload-len (u8vector-length payload-data)
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
        ) )
    (u8vector-append
      (u8vector
        (logior opcode (if fin? #x80 0))
        (logior payload-len-b1 (if masking-key #x80 0))
        )
      payload-len-bs
      (or masking-key '#u8())
      (if masking-key
        (mask-payload payload-data masking-key)
        payload-data)
      ) ) )

(define (parse-frame$
          :key
          [ on-parsed (errorf "on-parsed required") ]
          [ buffer-size-default 8000 ]
          [ buffer-size-limit 16384000 ]
          )
  (let [[ filled-bytes 0 ]
        [ parsed-bytes 0 ]
        [ buffer-size buffer-size-default ]
        ]
    (let* [[ b (make-u8vector buffer-size) ]
           [ reset-buffer!
             (lambda ()
               (when (> parsed-bytes 0)
                 (u8vector-copy! b 0
                                 b parsed-bytes filled-bytes
                                 )
                 (dec! filled-bytes parsed-bytes)
                 (set! parsed-bytes 0)
                 ) ) ]
           [ extend-buffer!
             (lambda ()
               (let1 new-size ($ min buffer-size-limit $ * 2 $ buffer-size)
                 (or (> new-size buffer-size)
                     (errorf "buffer limit exceeded")
                     )
                 (reset-buffer!)
                 (let1 new-b (make-u8vector new-size)
                   (u8vector-copy! new-b 0 b 0 filled-bytes)
                   (set! b new-b)
                   ) ) ) ]
           ]
      (lambda (in)
        (let loop []
	  (errorf "not implemented")
	  (loop)
          ) ) ) ) )
