(use gauche.test)
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

(test-end :exit-on-failure 1)
