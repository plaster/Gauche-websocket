(use gauche.test)
(use rfc.websocket)

(test-start "rfc.websocket")

(test-module 'rfc.websocket)

(define %table-opcode-symbol (with-module rfc.websocket %table-opcode-symbol))

(let [[ opcode-list (map car %table-opcode-symbol) ]
      [ opcode-symbol-list (map cdr %table-opcode-symbol) ]
      ]
  (for-each assert-opcode opcode-list)
  (for-each (^o (test-check o ($ opcode<-symbol $ symbol<-opcode o) =))
            opcode-list)
  (for-each (^s (test-check s ($ symbol<-opcode $ opcode<-symbol s) eq?))
            opcode-symbol-list)
  )

(test-end)
