#lang plait


(require (typed-in "backend.rkt"  [displayPrompt : (String -> Number)] [doEnd : ((Listof String) -> Void)] [getCustomAnswer : (  -> String)]))


(define-type Question
  (full (prompt : String) (respCount : Number) [numToResp :(Listof String)] (alternateKey : String ) (alternateQuestion : Question) (default : Question) (userSupplied? : Boolean) (rawQuestion : String))
  (end))

(define (responsesString list index needsOther?)
  (type-case (Listof String) list
    [(cons curr remaining) (string-append (to-string index) (string-append ". " (string-append  curr (string-append "\n" (responsesString remaining (+ 1 index) needsOther?)) ) )  )
]
    [empty (cond
             [needsOther? (string-append (to-string index) ". Other...\n"  )]
             [else ""])]))

(define (questionString prompt respCount numToResp needsOther?)
  (string-append  (string-append prompt "\n")
         (responsesString numToResp 1 needsOther?)))

(define (extractAnswer respCount numToResp toExtract canHaveOther? emergency)
  (cond
    [ (<=  toExtract respCount) (list-ref numToResp (-  toExtract 1))]
    [canHaveOther? (getCustomAnswer)]
    [else (emergency)]))

(define (extractResults res)
  (type-case (Listof String) res
    [(cons curr remaining) (string-append  (string-append "<br>" (extractResults remaining)) curr )]
    [empty ""]))

(define (printResults res)
  (doEnd (reverse res) ))



(define (printQ/getResults prompt respCount numToResp needsOther?)
  (extractAnswer respCount numToResp (displayPrompt (questionString prompt respCount numToResp needsOther?)) needsOther? (lambda () (printQ/getResults prompt respCount numToResp needsOther?) )))


(define (survey question responses)
  (type-case Question question
    [(full prompt respCount numToResp alternateKey alternateQuestion default needsOther? rawQ)
            (let ([answer (printQ/getResults  prompt respCount numToResp needsOther?)])
              (cond
                [(equal? answer alternateKey ) (survey alternateQuestion (cons (string-append (string-append rawQ " " ) answer ) responses))]
                [else (survey default (cons (string-append (string-append rawQ " " ) answer ) responses))]))]
    [(end) (printResults responses)]))


; (full (prompt : String) (respCount : Number) [numToResp :(Listof String)] (alternateKey : String ) (alternateQuestion : Question) (default : Question) )

(define ending (end))
(define question7 (full "To whom did Luke owe the most loyalty?" 4 (cons "his father" (cons "Obi Wan" (cons  "Han and Leia" (cons "Yoda" empty))))  "~~~~~~777" ending ending #t "To whom did Luke owe the most loyalty?"))
(define question6 (full "What in-universe explanation for Luke’s loss of hope in the sequel trilogy is most plausible?" 4 (cons "the pressure of building a new Jedi Order" (cons "unresolved fears about the Dark Side" (cons  "disillusionment with the history of the Jedi Order" (cons "new knowledge about midichlorians" empty))))  "~~~~~~777" ending ending #t "What in-universe explanation for Luke’s loss of hope in the sequel trilogy is most plausible?"))
(define question5 (full "Is Luke defined more by his hopefulness or his loyalty?" 2 (cons "his hopefulness" (cons "his loyalty" empty))  "his hopefulness" question6 question7 #f "Is Luke defined more by his hopefulness or his loyalty?"))
(define question4 (full "What was the root cause of Anakin’s downfall?" 4 (cons "his abilities" (cons "his pride in his abilities" (cons  "his relationship with Padme" (cons "his attachment to temporary things" empty))))  "Anakin Skywalker" ending ending #t "What was the root cause of Anakin’s downfall?"))
(define question3 (full "Whose arc do you find more satisfying?" 2 (cons "Anakin Skywalker" (cons "Luke Skywalker" empty))  "Anakin Skywalker" question4 question5 #f "Whose arc do you find more satisfying?") )
(define question2 (full "I’m amazed (and delighted!) at your choice. What about it bothers you the least?" 3 (cons "the words the actors say" (cons "how they say them" (cons "something else" empty)))  "~~~~~7777" ending question3 #f "What about it bothers you the least?"))
(define question1 (full "Which Star Wars trilogy is the best?" 3 (cons "The Original" (cons "The Prequel" (cons "The Sequel" empty)))  "The Prequel" question2 question3 #f "Which Star Wars trilogy is the best?"))










