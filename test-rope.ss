(module test-rope mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "util.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "comprehensions.ss" ("dyoo" "srfi-alias.plt" 1))
           (lib "etc.ss")
           (lib "list.ss")
           
           "rope.ss")
  
  (require/expose "rope.ss" (make-rope:concat))
  
  (define (make-long-degenerate-rope)
    (define (++ x y)
      (make-rope:concat x y (+ (rope-length x) (rope-length y))))
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
                      "2hello3hello4h")))))
  
  
  (test/text-ui rope-tests))