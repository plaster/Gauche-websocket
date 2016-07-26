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
                   $ + -1
                   $ min sz
                   $ u8vector-length buf) ) ) )

(define (open-chopped-input-uvector sz uv)
  ($ choppend-input-port sz
     $ open-input-uvector uv
     ) )

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
    (test "build-frame -> dispatch-parsed-frame$ text"
          input-text-list
          (^ ()
             (port-for-each values (pa$ parse in))
             output-text-list
             )
          equal?) ) )

(test-text "hoge")

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
    (test "build-frame -> dispatch-parsed-frame$ text"
          input-text-list
          (^ ()
             (port-for-each values (pa$ parse in))
             output-text-list
             )
          equal?) ) )

(test-chopped-text 100 "hoge")

(test-end :exit-on-failure 1)

;; vi:se expandtab:
