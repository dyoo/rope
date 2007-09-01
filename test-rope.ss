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
    (define my-rope (string->rope ""))
    (do-ec (:range i 5000)
           (set! my-rope (++ my-rope
                             (string->rope (format "hello~a" i)))))
    my-rope)
  
  
  (define (read-file-as-rope filename)
    (call-with-input-file filename
      (lambda (ip)
        (let loop ([char (read-char ip)]
                   [a-rope (string->rope "")])
          (cond [(eof-object? char)
                 a-rope]
                [else
                 (loop (read-char ip)
                       (rope-append a-rope
                                    (string->rope (string char))))])))))
  
  (define sr string->rope)
  
  (provide rope-tests)
  (define rope-tests
    (test-suite
     "rope.ss"
     (test-case
      "rope?"
      (check-false (rope? "is this a rope?"))
      (check-true (rope? (string->rope "is this a rope?")))
      (check-true (rope? (rope-append (string->rope "hello ")
                                      (string->rope "world")))))
     
     (test-case
      "rope-append"
      (check-equal? (rope->string (rope-append (sr "") (sr "abcd")))
                    (rope->string (rope-append (sr "abcd") (sr "")))))
     
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
      (parameterize ([current-optimize-flat-ropes #f])
        (check-equal? "abcdef"
                      (rope->string
                       (rope-balance (++ (sr "a")
                                         (++ (sr "bc")
                                             (++ (sr "d")
                                                 (sr "ef")))))))
        (check-equal? (rope-depth
                       (rope-balance (++ (sr "a")
                                         (++ (sr "bc")
                                             (++ (sr "d")
                                                 (sr "ef"))))))
                      2)))
     
     (test-case
      "rope-fold/leaves"
      (parameterize ([current-optimize-flat-ropes #f])
        (check-equal? (rope-fold/leaves (lambda (a-str acc)
                                          (cons a-str acc))
                                        '()
                                        (++ (sr "hello") (sr "world")))
                      (list "world" "hello"))))
     
     
     (test-case
      "rope-fold"
      (parameterize ([current-optimize-flat-ropes #f])
        (check-equal? (rope-fold (lambda (a-str acc)
                                   (cons a-str acc))
                                 '()
                                 (++ (sr "hello") (sr "world")))
                      (reverse
                       (list #\h #\e #\l #\l #\o
                             #\w #\o #\r #\l #\d)))))
     
     
     (test-case
      "open-input-rope"
      (parameterize ([current-optimize-flat-ropes #f])
        (check-equal?
         (regexp-match
          "abracadabra"
          (open-input-rope
           (++ (sr "a") (++ (sr "braca")
                            (++ (++ (sr "da") (sr "br")) (sr "a"))))))
         '(#"abracadabra"))))
     
     
     (test-case
      "rope-depth and balancing"
      (parameterize ([current-optimize-flat-ropes #f])
        (check-equal? (rope-depth (rope-balance (++ (sr "h0")
                                                    (sr "h1"))))
                      1)
        (check-equal? (rope-depth (rope-balance (++ (sr "h0")
                                                    (++ (sr "h1")
                                                        (sr "h2")))))
                      2)
        (check-equal?
         (rope-depth (rope-balance (++ (sr "h0")
                                       (++ (sr "h1")
                                           (++ (sr "h2") (sr "h3"))))))
         2)
        (check-equal? (rope-depth
                       (rope-balance
                        (++ (sr "h0") (++ (sr "h1")
                                          (++ (sr "h2")
                                              (++ (sr "h3")
                                                  (sr "h4")))))))
                      3)
        (check-equal?
         (rope-depth
          (rope-balance
           (++ (sr "h0")
               (++ (sr "h1")
                   (++ (sr "h2")
                       (++ (sr "h3") (++ (sr "h4") (sr "h5"))))))))
         3)))
     
     
     (test-case
      "rope-ref"
      (parameterize ([current-optimize-flat-ropes #f])
        (local ((define word-rope (++
                                   (++ (++ (sr "super")
                                           (sr "cali"))
                                       (++ (sr "fragil")
                                           (sr "istic")))
                                   (++ (sr "expiali")
                                       (sr "docious"))))
                (define word-string "supercalifragilisticexpialidocious"))
          (for-each (lambda (i ch)
                      (check-equal? (rope-ref word-rope i) ch))
                    (build-list (string-length word-string) (lambda (i) i))
                    (string->list word-string)))))))
  
  
  (test/text-ui rope-tests))