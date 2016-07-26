(define-module rfc.websocket
  (use gauche.uvector)
  (use gauche.parameter)
  (use srfi-11)
  (use util.match)
  (export
    opcode<-symbol
    symbol<-opcode
    opcode-symbol?
    opcode?
    assert-opcode
    opcode-continue
    opcode-text
    opcode-binary
    opcode-close
    opcode-ping
    opcode-pong

    build-frame

    *parse-buffer-size-default*
    *parse-buffer-size-limit*

    parse-frame$
    dispatch-frame$

    dispatch-parsed-frame$$
    dispatch-parsed-frame$
    ) )
(select-module rfc.websocket)

;;;; protocol specification: RFC 6455
;;;; https://tools.ietf.org/html/rfc6455

(define %table-opcode-symbol
  '(( #x0 . continue )
    ( #x1 . text )
    ( #x2 . binary )
    ;; %x3-7 are reserved for further non-control frames
    ( #x8 . close )
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

(define opcode-continue      (opcode<-symbol 'continue      ))
(define opcode-text          (opcode<-symbol 'text          ))
(define opcode-binary        (opcode<-symbol 'binary        ))
(define opcode-close         (opcode<-symbol 'close         ))
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
         (values payload-len '#u8()) ]
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

(define *parse-buffer-size-default* (make-parameter     8000))
(define *parse-buffer-size-limit*   (make-parameter 16384000))

(define (%decode-u64<-u8vector u8)
  (logior (ash (u8vector-ref u8 0) 56)
	  (ash (u8vector-ref u8 1) 48)
	  (ash (u8vector-ref u8 2) 40)
	  (ash (u8vector-ref u8 3) 32)
	  (ash (u8vector-ref u8 4) 24)
	  (ash (u8vector-ref u8 5) 16)
	  (ash (u8vector-ref u8 6) 8)
	  (ash (u8vector-ref u8 7) 0)
	  ))
(define (%decode-u16<-u8vector u8)
  (logior (ash (u8vector-ref u8 0) 8)
	  (ash (u8vector-ref u8 1) 0)
	  ))

(define (parse-frame$
          :key
          [ on-parsed (errorf "on-parsed required") ]
          [ buffer-size-default (*parse-buffer-size-default*) ]
          [ buffer-size-limit (*parse-buffer-size-limit*) ]
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
	   [ space-buffer!
	     (lambda (n)
	       (if (> n (- (u8vector-length b)
			   (- filled-bytes parsed-bytes)))
		 (extend-buffer!)
		 (reset-buffer!)
		 ) ) ]
           [ recv-buffer!
             (lambda (in)
               (let1 r (read-uvector! b in filled-bytes)
                 (cond
                   [ (eof-object? r) r ]
                   [ (= 0 r)         r ]
                   [else
                     (inc! filled-bytes r)
		     r
                     ] ) ) ) ]
           [ consume-buffer!
             (lambda (n)
               (inc! parsed-bytes n) ) ]
           [ peek-buffer
             (lambda (n :optional [skip 0])
               (and (>= (+ skip n) (- filled-bytes parsed-bytes))
                    (u8vector-copy b skip (+ skip n)) ) ) ]
           ]
      (lambda (in)
        (match (recv-buffer! in)
          [ (and (? eof-object? ) r) r ]
	  [ 0 0 ]
          [ _
	    (let/cc return
              (let loop [[ p 0 ]]
		(define (peek-buffer* n :optional [skip 0])
		  (or (peek-buffer n skip)
		      (return p) ) )
                (match (u8vector->vector (peek-buffer* 2))
                  [ #f
	            (space-buffer! 2)
	            #f ]
                  [ #(b0 b1)
                    (let [[ fin? (logbit? b0 7) ]
                          [ opcode (logand b0 #x7F) ]
                          [ masked? (logbit? b1 7) ]
                          [ payload-len-b1 (logand b1 #x7F) ]
			  [ skip 2 ]
                          ]
                      (let*-values
			[[ (skip payload-length)
			  (case payload-len-b1
			    [ ( 127 ) (values (+ 4 skip) (%decode-u64<-u8vector (peek-buffer* 4 skip))) ]
			    ;; TODO: assert that payload-length <= #x7FFFFFFFFFFFFFFF
			    [ ( 126 ) (values (+ 2 skip) (%decode-u16<-u8vector (peek-buffer* 2 skip))) ]
			    [else (values skip payload-len-b1) ]
			    ) ]
			 [ (skip masking-key)
			  (if masked?
			    (values (+ 4 skip) (peek-buffer* 4 skip))
			    (values skip #f)
			    ) ]
			 [ (skip payload-data)
			  (values (+ payload-length skip)
				  (peek-buffer* payload-length skip)) ]
			     ]
			(on-parsed :fin? fin?
				   :opcode opcode
				   :masking-key masking-key
				   :payload-data payload-data
				   )
			(consume-buffer! skip)
                        (loop (+ p 1))
                        ) ) ]
                  ) ) ) ] ) ) ) ) )

(define (dispatch-frame$
	  :key
	  [ on-ping #f ]
	  [ on-pong #f ]
	  [ on-binary #f ]
	  [ on-text #f ]
	  [ on-close #f ]
	  [ [ :init-cont-state cont-state ] #f]
	  )
  (lambda (:key
	    [ fin? (errorf "~s required" :fin?) ]
	    [ opcode (errorf "~s required" :opcode) ]
	    [ masking-key  (errorf "~s required" :masking-key) ]
	    [ payload-data (errorf "~s required" :payload-data) ]
	    )
    (define (flush)
      (receive (opcode-symbol plain-payload-data)
        (match cont-state
	       [ (opcode-symbol plain-payload-data)
		(values opcode-symbol plain-payload-data) ]
	       [ (opcode-symbol . r-plain-payload-data-list)
		($ values opcode-symbol
		   $ u8vector-append
		   $* reverse r-plain-payload-data-list) ]
	       )
	(set! cont-state #f)
	(case opcode-symbol
	  [ (text) (and on-text ($ on-text $ u8vector->string plain-payload-data)) ]
	  [ (binary) (and on-binary (on-binary plain-payload-data)) ]
	  [ else
	    (errorf "internal error: unknown opcode-symbol: ~s" opcode-symbol) ] )
	) )

    (case (symbol<-opcode opcode)
      [ ( text binary )
       => (^ (opcode-symbol)
	    (and cont-state (errorf "continuation frame expected"))
	    (and masking-key
		 (mask-payload! payload-data masking-key) )
	    (set! cont-state `(,opcode-symbol ,payload-data))
	    (and fin? (flush))
	    ) ]
      [ ( continue )
       (or cont-state (errorf "continuation frame is NOT expected"))
       (and masking-key
	    (mask-payload! payload-data masking-key) )
       (match cont-state
	 [ (opcode-symbol . r-plain-payload-data-list)
	  (set! cont-state `(,opcode-symbol . (,payload-data . ,r-plain-payload-data-list)))
	  ] )
       (and fin? (flush))
       ]
      [ ( ping )
       (on-ping)
       ]
      [ ( pong )
       (on-pong)
       ]
      [ ( close )
       (on-close)
       ]
      [ ( #f )
       (errorf "unknown opcode: ~s" opcode)
       ]
      [ else => (^ (opcode-symbol)
		  (errorf "internal error: unknown opcode-symbol: ~s" opcode-symbol) ) ]
      ) ) )

(define ((dispatch-parsed-frame$$ . parse-frame$-args) . dispatch-frame$-args)
  (apply parse-frame$
	 :on-parsed (apply dispatch-frame$ dispatch-frame$-args)
	 parse-frame$-args))

(define dispatch-parsed-frame$ (dispatch-parsed-frame$$))
