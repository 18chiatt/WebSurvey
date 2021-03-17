#lang racket/base
(provide (all-defined-out) )


(define (doEnd results)
  (send/suspend (lambda (where-next)
                  (response/xexpr
  `(html (head (title "Hello world!"))
          (body (h1 "Your Answers")
                      ,@(map
         (λ (result)
           `(p ,result))
         results)
                ))  ))
   )
  )



(require web-server/http/xexpr
         web-server/http/request-structs ; for bindings
         web-server/servlet/web) ; for send/suspend

(define (getxexpr prompt where-to-send-next)(response/xexpr
  `(html (head (title "Hello world!"))
          (body (p ,prompt)
                (form ([action ,where-to-send-next]
                       [method "POST"])
                       (input ([type "text"]
                               [name "number"])))))  ))

(define (extractResponse nextReq)
  (string->number
                (bytes->string/utf-8
                 (binding:form-value
                  (bindings-assq
                   #"number"
                   (request-bindings/raw nextReq)))))
  )

(define (extractString nextReq)
                (bytes->string/utf-8
                 (binding:form-value
                  (bindings-assq
                   #"number"
                   (request-bindings/raw nextReq))))
  )


(define (getCustomAnswer)
    (let ([nextReq (send/suspend
   (lambda (where-to-send-next)
     (getxexpr "Your Answer:" where-to-send-next)))])
    (let ([num (extractString nextReq)])
      (if num
        num
        (display "massive error, not a string ??")))
    ))

(define (displayPrompt prompt )
  (let ([nextReq (send/suspend
   (lambda (where-to-send-next)
     (getxexpr prompt where-to-send-next)))])
    (let ([num (extractResponse nextReq)])
      (if num
        num
        (display "massive error, not a number!")))
    )
  )


