(module text-get-rope mzscheme
  
  ;; Provides a text% mixin that adds get-rope and set-rope methods.
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "lex.ss" "parser-tools")
           "rope.ss")
  
  (provide rope-mixin)
  
  (define (rope-mixin super%)
    (class super%
      (inherit begin-edit-sequence
               end-edit-sequence
               erase
               insert
               delete)
      (define rope (string->rope ""))
      
      (define/public (get-rope)
        rope)
      
      (define/public (set-rope a-rope)
        ;; Really inefficient. Fixme by using edit distance (levenshein)
        (begin-edit-sequence)
        (erase)
        (rope-fold/leaves (lambda (snip _)
                            (insert snip))
                          #f
                          a-rope)
        (set! rope a-rope)
        (end-edit-sequence))
      
      
      (define/augment (after-delete start len)
        (inner #f after-delete start len)
        (set! rope (rope-append (subrope rope 0 start) (subrope rope (+ start len)))))
      
      (define/augment (after-insert start len)
        (inner #f after-insert start len)
        (set! rope (rope-append
                    (rope-append (subrope rope 0 start)
                                 (read-subrope this start len))
                    (subrope rope start))))
      
      (super-new)))
  
  
  
  (define (read-subrope text start len)
    (local
        (
         ;; Simple lexer: pulls ropes from a port. Assumes specials
         ;; should be unboxed.
         (define mylexer
           (lexer
            [(repetition 0 +inf.0 any-char)
             (string->rope lexeme)]
            [(special)
             (special->rope (unbox lexeme))]
            [(eof) eof]))
         
         (define ip (open-input-text-editor text start (+ start len)
                                            (lambda (snip) (box snip))
                                            #f #f)))
      (let loop ([inserted-rope (string->rope "")]
                 [next-chunk (mylexer ip)])
        (cond
          [(eof-object? next-chunk)
           inserted-rope]
          [else
           (loop (rope-append inserted-rope next-chunk)
                 (mylexer ip))]))))
  
  
  (define (open-file filename)
    (local ((define f (make-object frame% "test" #f 400 500))
            (define t (make-object (rope-mixin text%)))
            (define c (make-object editor-canvas% f t '(no-hscroll))))
      (send t load-file filename)
      (send t auto-wrap #t)
      (send f show #t)
      t))
  
  
  )