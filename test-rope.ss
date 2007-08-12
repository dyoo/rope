(module test-rope mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "util.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "comprehensions.ss" ("dyoo" "srfi-alias.plt" 1))
           (lib "etc.ss")
           (lib "list.ss")
           
           "rope.ss")
  
  (require/expose "rope.ss" (make-rope:concat))
  (define (++ x y)
    (make-rope:concat x y (+ (rope-length x) (rope-length y))))
  
  
  (define (make-long-degenerate-rope)
    (define my-rope "")
    (do-ec (:range i 5000)
           (set! my-rope (++ my-rope (format "hello~a" i))))
    my-rope)
  
  
  
  (define (read-file-as-rope filename)
    (call-with-input-file filename
      (lambda (ip)
        (let loop ([char (read-char ip)]
                   [a-rope ""])
          (cond [(eof-object? char)
                 a-rope]
                [else
                 (loop (read-char ip)
                       (rope-append a-rope (string char)))])))))
  
  (provide rope-tests)
  (define rope-tests
    (test-suite
     "rope.ss"
     (test-case
      "rope?"
      (check-true (rope? "is this a rope?"))
      (check-true (rope? (rope-append "hello " "world"))))
     
     (test-case
      "rope-append"
      (check-equal? (rope->string (rope-append "" "abcd"))
                    (rope->string (rope-append "abcd" ""))))
     
     (test-case
      "subrope checking bounds"
      (local ((define myrope (make-long-degenerate-rope)))
        (check-equal? (rope->string
                      (subrope myrope 0 18))
                     "hello0hello1hello2")
        (check-equal? (rope->string
                       (subrope myrope 1 18))
                      "ello0hello1hello2")
        (check-equal? (rope->string
                       (subrope myrope 3 18))
                      "lo0hello1hello2")
        (check-equal? (rope->string
                       (subrope myrope 6 18))
                      "hello1hello2")
        (check-equal? (rope->string
                       (subrope myrope 6 15))
                      "hello1hel")
        (check-equal? (rope->string
                       (subrope myrope 17 30))
                      "2hello3hello4")
        (check-equal? (rope->string
                       (subrope myrope 17 31))
                      "2hello3hello4h")))
     
     (test-case
      "balance"
      (check-equal? "abcdef"
                    (rope->string (rope-balance (++ "a" (++ "bc" (++ "d" "ef"))))))
      (check-equal? (rope-depth (rope-balance (++ "a" (++ "bc" (++ "d" "ef")))))
                    2))
     
     (test-case
      "rope-fold/leaves"
      (check-equal? (rope-fold/leaves (lambda (a-str acc)
                                        (cons a-str acc))
                                      '()
                                      (++ "hello" "world"))
                    (list "world" "hello")))
     
     
     (test-case
      "rope-fold"
      (check-equal? (rope-fold (lambda (a-str acc)
                                 (cons a-str acc))
                               '()
                               (++ "hello" "world"))
                    (reverse (list #\h #\e #\l #\l #\o #\w #\o #\r #\l #\d))))
     
     (test-case
      "open-input-rope"
      (check-equal?
       (regexp-match
        "abracadabra"
        (open-input-rope (++ "a" (++ "braca" (++ (++ "da" "br") "a")))))
       '(#"abracadabra")))
     
     
     (test-case
      "rope-depth and balancing"
      (check-equal? 1 (rope-depth
                       (rope-balance (++ "h0" "h1"))))
      (check-equal? 2 (rope-depth
                       (rope-balance (++ "h0" (++ "h1" "h2")))))
      (check-equal? 2 (rope-depth
                       (rope-balance (++ "h0" (++ "h1" (++ "h2" "h3")))))))))
  
  
  (test/text-ui rope-tests))