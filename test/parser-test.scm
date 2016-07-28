(use gauche.test)
(use gauche.uvector)
(use gauche.vport)
(use rfc.websocket)

(test-start "rfc.websocket")

(test-module 'rfc.websocket)

(define %table-opcode-symbol (with-module rfc.websocket %table-opcode-symbol))

(let [[ opcode-list (map car %table-opcode-symbol) ]
      [ opcode-symbol-list (map cdr %table-opcode-symbol) ]
      ]
  (for-each assert-opcode opcode-list)
  (for-each (^o (test "opcode -> symbol -> opcode" o (^() ($ opcode<-symbol $ symbol<-opcode o)) =))
            opcode-list)
  (for-each (^s (test "symbol -> opcode -> symbol" s (^() ($ symbol<-opcode $ opcode<-symbol s)) eq?))
            opcode-symbol-list)
  )

(test "build-frame text"
      '#u8(#x81 4 104 111 103 101)
      (pa$ build-frame
           :fin? #t
           :opcode opcode-text
           :payload-data (string->u8vector "hoge")))


(define (choppend-input-port sz in)
  (make <buffered-input-port>
        :fill (^ (buf)
                ($ read-uvector! buf in 0
                   $ min sz
                   $ u8vector-length buf) ) ) )

(define (open-chopped-input-uvector sz uv)
  ($ choppend-input-port sz
     $ open-input-uvector uv
     ) )

(define (test-frames . frame-spec-list)
  (let* [[ output-frame-list '() ]
         [ parse
           (parse-frame$ :on-parsed (.$ (cut push! output-frame-list <>) list))
           ]
         [ input-frame-list
           (map (apply$ build-frame) frame-spec-list)
           ]
         [ in (open-input-uvector (apply u8vector-append input-frame-list)) ]
         ]
    (port-for-each values (pa$ parse in))
    (test "build-frame -> parse-frame$"
          frame-spec-list
          (cut reverse output-frame-list)
          equal?)
    ))

(define (test-chopped-frames chop-size . frame-spec-list)
  (let* [[ output-frame-list '() ]
         [ parse
           (parse-frame$ :on-parsed (.$ (cut push! output-frame-list <>) list))
           ]
         [ input-frame-list
           (map (apply$ build-frame) frame-spec-list)
           ]
         [ in (open-chopped-input-uvector chop-size (apply u8vector-append input-frame-list)) ]
         ]
    (port-for-each values (pa$ parse in))
    (test #`"build-frame -> parse-frame$ chop ,chop-size"
          frame-spec-list
          (cut reverse output-frame-list)
          equal?)
    ))

(test-frames `(:fin? #t :opcode ,opcode-binary :masking-key #f :payload-data #u8( 5 4 3 2 )))
(test-frames `(:fin? #f :opcode ,opcode-binary :masking-key #f :payload-data #u8( 5 4 3 2 )))
(test-frames `(:fin? #f :opcode ,opcode-continue :masking-key #f :payload-data #u8( 5 4 3 2 )))
(test-frames `(:fin? #t :opcode ,opcode-binary :masking-key #f :payload-data #u8( 5 4 3 2 ))
             `(:fin? #t :opcode ,opcode-binary :masking-key #f :payload-data #u8( 5 6 7 8 ))
             )

(test-chopped-frames 1
  `(:fin? #t :opcode ,opcode-binary :masking-key #f :payload-data #u8( 5 4 3 2 )))
(test-chopped-frames 2
  `(:fin? #t :opcode ,opcode-binary :masking-key #f :payload-data #u8( 5 4 3 2 )))
(test-chopped-frames 10
  `(:fin? #t :opcode ,opcode-binary :masking-key #f :payload-data #u8( 5 4 3 2 )))

(define (test-text . input-text-list)
  (let* [[ output-text-list '() ]
         [ parse (dispatch-parsed-frame$
                   :on-text (cut push! output-text-list <> )
                   ) ]
         [ in
           ($ open-input-uvector
              $ u8vector-append
              $* map
              ($ build-frame
                 :fin? #t
                 :opcode opcode-text
                 :payload-data $ string->u8vector $)
              input-text-list
              ) ]
         ]
    (port-for-each values (pa$ parse in))
    (test "build-frame -> dispatch-parsed-frame$ text"
          input-text-list
          (cut reverse output-text-list)
          equal?) ) )

(test-text "hoge")
(test-text "hoge" "piyo" "pospos")

(define (test-chopped-text chop-size . input-text-list)
  (let* [[ output-text-list '() ]
         [ parse (dispatch-parsed-frame$
                   :on-text (cut push! output-text-list <> )
                   ) ]
         [ in
           ($ open-chopped-input-uvector chop-size
              $ u8vector-append
              $* map
              ($ build-frame
                 :fin? #t
                 :opcode opcode-text
                 :payload-data $ string->u8vector $)
              input-text-list
              ) ]
         ]
    (port-for-each values (pa$ parse in))
    (test "build-frame -> dispatch-parsed-frame$ text"
          input-text-list
          (cut reverse output-text-list)
          equal?) ) )

(test-chopped-text 100 "hoge")

(test-end :exit-on-failure 1)

;; vi:se expandtab:
