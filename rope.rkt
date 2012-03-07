#lang racket/base
(require racket/match
         racket/contract
         racket/local
         racket/port
         parser-tools/lex
         (planet dyoo/join-forest:1:2/join-forest)
         "immutable-string.ss")

;; Quick and dirty library implementing ropes, closely following
;; the description in: 
;;
;; H. Boehm, R. Atkinson, M. Plass.
;; 'Ropes: An Alternative to Strings'
;; Software --- Practice and Experience, Vol 25(12), 1315-1330.
;; (December 1995)
;;
;; Slight extension: rope elements are allowed to be non-string
;; special data, in which case rope->string can not be applied.


;; A rope is either a string-node, a special-node, or a rope:concat.
(define-struct rope ())
(define-struct (rope:string rope) (s))
(define-struct (rope:special rope) (s))
(define-struct (rope:concat rope) (l r len depth))

;; A leaf-node is considered to be a string-node or special-node.


;; We often use the empty string rope for base cases, so let me
;; define it here.
(define rope-empty
  (make-rope:string (string->immutable-string "")))


;; Arbitrary length cutoff until we allocate a new concat node
;; TODO: experiment to see what value is good for this.
(define cutoff-before-concat-node-use 32)

;; below-flat-collapsing-cutoff?: string string -> boolean
(define (below-flat-collapsing-cutoff? s1 s2)
  (and (current-optimize-flat-ropes)
       (< (+ (string-length s1) (string-length s2))
          cutoff-before-concat-node-use)))

;; Currently undocumented, but if current-optimize-flat-ropes
;; is off, then we won't try to optimize the appending of
;; consecutive string nodes.
(define current-optimize-flat-ropes (make-parameter #t))


;; string->rope: string -> rope
;; Given a string, returns a rope.
(define (string->rope a-str)
  (let loop ([i 0]
             [acc '()])
    (cond
     [(< (+ i cutoff-before-concat-node-use)
         (string-length a-str))
      (loop (+ i cutoff-before-concat-node-use)
            (cons (make-rope:string
                   (immutable-substring
                    a-str i (+ i cutoff-before-concat-node-use)))
                  acc))]
     [else
      (simple-join-forest (reverse
                           (cons (make-rope:string
                                  (immutable-substring a-str i))
                                 acc))
                          -rope-append)])))

(define special->rope make-rope:special)

;; rope-length: rope -> number
;; Returns the length of a rope
(define (rope-length a-rope)
  (match a-rope
    [(struct rope:string (s))
     (string-length s)]
    [(struct rope:special (s))
     1]
    [(struct rope:concat (l r len depth))
     len]))




;; rope-has-special? rope -> boolean
;; Returns true if the rope has a special.
(define (rope-has-special? a-rope)
  (match a-rope
    [(struct rope:string (s)) #f]
    [(struct rope:special (s)) #t]
    [(struct rope:concat (l r len depth))
     (or (rope-has-special? l)
         (rope-has-special? r))]))


(define current-max-depth-before-rebalancing
  (make-parameter 32))

;; rebalance-if-too-deep: rope -> rope
;; Automatically rebalance the rope if the depth is beyond
;; the requested threshold.
(define (rebalance-if-too-deep a-rope)
  (cond
   [(< (rope-depth a-rope)
       (current-max-depth-before-rebalancing))
    a-rope]
   [else
    (rope-balance a-rope)]))


;; -rope-append: rope rope -> rope
;; Puts two ropes together; if the rope depth is bad enough,
;; call rope-balance implicitly.
(define (rope-append rope-1 rope-2)
  (rebalance-if-too-deep (-rope-append rope-1 rope-2)))


;; -rope-append: rope rope -> rope
;; Puts two ropes together.
(define (-rope-append rope-1 rope-2)
  (local ((define (make-default-concat r1 r2)
            (cond
             [(= 0 (rope-length r1))
              r2]
             [(= 0 (rope-length r2))
              r1]
             [else
              (make-rope:concat r1 r2
                                (+ (rope-length r1)
                                   (rope-length r2))
                                (add1 (max (rope-depth r1)
                                           (rope-depth r2))))])))
         (match (list rope-1 rope-2)
           [(list (struct rope:string (s1))
                  (struct rope:string (s2)))
            (cond
             [(below-flat-collapsing-cutoff? s1 s2)
              (make-rope:string (immutable-string-append s1 s2))]
             [else
              (make-default-concat rope-1 rope-2)])]
           
           [(list (struct rope:concat
                          (left-rope
                           (struct rope:string (s1))
                           len
                           depth))
                  (struct rope:string (s2)))
            (cond
             [(below-flat-collapsing-cutoff? s1 s2)
              (make-rope:concat
               left-rope
               (make-rope:string (immutable-string-append s1 s2))
               (+ (rope-length rope-1) (rope-length rope-2))
               (add1 (rope-depth left-rope)))]
             
             [else
              (make-default-concat rope-1 rope-2)])]
           
           [(list (struct rope:string (s1))
                  (struct rope:concat
                          ((struct rope:string (s2))
                           right-rope
                           len
                           depth)))
            (cond
             [(below-flat-collapsing-cutoff? s1 s2)
              (make-rope:concat
               (make-rope:string (immutable-string-append s1 s2))
               right-rope
               (+ (rope-length rope-1) (rope-length rope-2))
               (add1 (rope-depth right-rope)))]
             
             [else
              (make-default-concat rope-1 rope-2)])]
           
           
           [else
            (make-default-concat rope-1 rope-2)])))


;; rope-append*: rope* -> rope
;; Appends a sequence of ropes into a single rope.
(define (rope-append* . some-ropes)
  (rebalance-if-too-deep
   (simple-join-forest (cons rope-empty some-ropes) -rope-append)))


;; rope-ref: rope number -> character
;; Gets the nth character of a-rope.
(define (rope-ref a-rope index)
  (match a-rope
    [(struct rope:string (s))
     (string-ref s index)]
    [(struct rope:special (s))
     s]
    [(struct rope:concat (l r len depth))
     (local ((define l-length (rope-length l)))
            (cond
             [(< index l-length)
              (rope-ref l index)]
             [else
              (rope-ref r (- index l-length))]))]))


;; subrope: rope number number -> rope
;; Takes a subsequence of the rope from start,
;; up to (but not including) end.
(define subrope
  (local ((define (subrope a-rope start end)
            (match a-rope
              [(struct rope:string (s))
               (make-rope:string
                (immutable-substring s start end))]
              
              [(struct rope:special (s))
               (cond [(= start end)
                      rope-empty]
                     [else
                      a-rope])]
              
              [(struct rope:concat (rope-1 rope-2 len depth))
               (local
                ((define length-of-rope-1 (rope-length rope-1))
                 (define left
                   (cond
                    [(and (<= start 0)
                          (<= length-of-rope-1 end))
                     rope-1]
                    [(<= length-of-rope-1 start)
                     rope-empty]
                    [else
                     (subrope rope-1
                              (min start length-of-rope-1)
                              (min end length-of-rope-1))]))
                 (define right
                   (cond
                    [(and (<= start length-of-rope-1)
                          (<= len end))
                     rope-2]
                    [(<= end length-of-rope-1)
                     rope-empty]
                    [else
                     (subrope rope-2
                              (max 0 (- start length-of-rope-1))
                              (max 0 (- end
                                        length-of-rope-1)))])))
                (-rope-append left right))]))
          
          (define (clamp x low high)
            (min (max x low) high)))
         
         (case-lambda
           [(a-rope start)
            (subrope a-rope
                     (clamp start 0 (rope-length a-rope))
                     (rope-length a-rope))]
           [(a-rope start end)
            (cond [(<= start end)
                   (subrope a-rope
                            (clamp start 0 (rope-length a-rope))
                            (clamp end 0 (rope-length a-rope)))]
                  [else
                   (error 'subrope
                          "end greater than start" start end)])])))

;; rope=?: rope rope -> boolean
;; Returns true if the two ropes have the same content.
(define (rope=? rope-1 rope-2)
  (cond
   [(eq? rope-1 rope-2)
    #t]
   [(not (= (rope-length rope-1)
            (rope-length rope-2)))
    #f]
   [else
    ;; Ugly case analysis ahead: we've got
    ;; to cover all the cases if we want to avoid
    ;; the default that uses rope->vector.
    (match (list rope-1 rope-2)
      [(list (struct rope:string (s1))
             (struct rope:string (s2)))
       (string=? s1 s2)]
      
      [(list (struct rope:string (s1))
             (struct rope:special (s2)))
       #f]
      
      [(list (struct rope:string (s1))
             (struct rope:concat (l2 r2 len2 depth2)))
       (let/ec return
         (= len2
            (rope-fold (lambda (ch/special i)
                         (cond
                          [(and (char? ch/special)
                                (char=? ch/special
                                        (string-ref s1 i)))
                           (add1 i)]
                          [else
                           (return #f)]))
                       0
                       rope-2)))]
      
      [(list (struct rope:special (s1))
             (struct rope:string (s2)))
       #f]
      
      [(list (struct rope:special (s1))
             (struct rope:special (s2)))
       (eq? s1 s2)]
      
      [(list (struct rope:special (s1))
             (struct rope:concat (l2 r2 len2 depth2)))
       (or (rope=? rope-1 l2)
           (rope=? rope-1 r2))]
      
      [(list (struct rope:concat (l1 r1 len1 depth1))
             (struct rope:string (s2)))
       (rope=? rope-2 rope-1)]
      
      [(list (struct rope:concat (l1 r1 len1 depth1))
             (struct rope:special (s2)))
       (rope=? rope-2 rope-1)]
      
      [(list (struct rope:concat (l1 r1 len1 depth1))
             (struct rope:concat (l2 r2 len2 depth2)))
       (cond [(= (rope-length l1) (rope-length l2))
              (and (rope=? l1 l2)
                   (rope=? r1 r2))]
             [else
              (equal? (rope->vector rope-1)
                      (rope->vector rope-2))])])]))



;; rope->string: rope -> string
;; Gets a string from the rope.
(define (rope->string a-rope)
  (let ([target (make-string (rope-length a-rope))])
    (let loop! ([a-rope a-rope]
                [i 0])
      (match a-rope
        [(struct rope:string (s))
         (string-copy! target i s)
         (+ i (string-length s))]
        [(struct rope:special (s))
         (error 'rope->string "rope contains special ~s" s)]
        [(struct rope:concat (l r len depth))
         (loop! r (loop! l i))]))
    target))


;; rope-for-each: (char -> void) rope -> void
;; Iterates a function f across each character in the rope.
(define (rope-for-each f a-rope)
  (rope-fold (lambda (ch acc) (f ch)) (void) a-rope))


;; fope-fold: (char X -> X) X rope -> X
;; Folds a character-consuming accumulator across the characters
;; in the rope.
(define (rope-fold f acc a-rope)
  (match a-rope
    [(struct rope:string (s))
     (for/fold ([acc acc])
               ([ch (in-string s)])
       (f ch acc))]
    [(struct rope:special (s))
     (f s acc)]
    [(struct rope:concat (l r len depth))
     (rope-fold f (rope-fold f acc l) r)]))


;; rope-fold/leaves: (rope:leaf X -> X) X rope -> X
;; Recent api change 3.0: this no longer unboxes the structure
;; before calling the function on the rope.
(define (rope-fold/leaves f acc a-rope)
  (match a-rope
    [(struct rope:string (s))
     (f a-rope acc)]
    [(struct rope:special (s))
     (f a-rope acc)]
    [(struct rope:concat (l r len depth))
     (rope-fold/leaves f (rope-fold/leaves f acc l) r)]))


;; open-input-rope: rope -> input-port
;; Opens an input port using the characters in the rope.
(define (open-input-rope a-rope)
  (cond
   ;; Builds a pipe for input and output. We do some logic here
   ;; because if we don't have specials, then
   ;; we can take advantage of it.
   [(rope-has-special? a-rope)
    (local ((define-values (inp outp)
              (make-pipe-with-specials)))
           (rope-fold/leaves (lambda (string/special _)
                               (match string/special
                                 [(struct rope:string (s))
                                  (when (> (string-length s) 0)
                                    (display s outp))]
                                 [(struct rope:special (s))
                                  (write-special s outp)]))
                             #f
                             a-rope)
           (close-output-port outp)
           inp)]
   [else
    (open-input-string (rope->string a-rope))]))


;; rope-balance: rope -> rope
;; Reconcatenate the leaves of the rope.
(define (rope-balance a-rope)
  (fib-join-forest (reverse
                    (rope-fold/leaves cons '() a-rope))
                   -rope-append
                   rope-depth))




;; rope->vector: rope -> (vectorof char-or-special)
;; Given a rope, returns a vector containing all of its items.
(define (rope->vector a-rope)
  (local ((define vec (make-vector (rope-length a-rope))))
         (rope-fold (lambda (char-or-special index)
                      (vector-set! vec index char-or-special)
                      (add1 index))
                    0
                    a-rope)
         vec))


;; vector->rope: (vectorof char special) -> rope
;; Inverts rope->vector.
(define (vector->rope a-vec)
  (let loop ([i 0]
             [acc (string->rope "")])
    (cond [(= i (vector-length a-vec))
           acc]
          [(char? (vector-ref a-vec i))
           (loop (add1 i)
                 (-rope-append
                  acc
                  (string->rope (string (vector-ref a-vec i)))))]
          [else
           (loop (add1 i)
                 (-rope-append
                  acc
                  (special->rope (vector-ref a-vec i))))])))



;; input-port->rope: input-port (special -> special) -> rope
;; Returns a rope whose content contains the content of the input port.
;; Specials are preprocessed with handle-special-f before making them
;; rope:special nodes.
(define (input-port->rope ip handle-special-f)
  (local [(define simple-lexer
            (lexer
             [(repetition 0 +inf.0 any-char)
              lexeme]
             [(special)
              (box lexeme)]
             [(eof) eof]))]
         (lambda (ip handle-special-f)
           (let loop ([inserted-rope (string->rope "")]
                      [next-chunk (simple-lexer ip)])
             (cond
              [(eof-object? next-chunk)
               inserted-rope]
              [(string? next-chunk)
               (loop (rope-append inserted-rope
                                  (string->rope next-chunk))
                     (simple-lexer ip))]
              [(box? next-chunk)
               (loop (rope-append inserted-rope
                                  (special->rope
                                   (handle-special-f
                                    (unbox next-chunk))))
                     (simple-lexer ip))])))))



;; rope-depth: rope -> natural-number
(define (rope-depth a-rope)
  (match a-rope
    [(struct rope:string (s))
     0]
    [(struct rope:special (s))
     0]
    [(struct rope:concat (l r len depth))
     depth]))


;; rope-node-count: rope -> natural-number
;; Counts how many nodes (both leaves and concat nodes) are
;; in the rope.
;; Just for debugging.
(define (rope-node-count a-rope)
  (match a-rope
    [(struct rope:string (s))
     1]
    [(struct rope:special (s))
     1]
    [(struct rope:concat (l r len depth))
     (add1 (+ (rope-node-count l)
              (rope-node-count r)))]))


;; Here are our exposed functions:

(provide current-optimize-flat-ropes
         current-max-depth-before-rebalancing)

(provide
 (contract-out
 [struct rope []]
 [struct (rope:string rope) [(s (and/c string? immutable?))]]
 [struct (rope:special rope) [(s any/c)]]
 [struct (rope:concat rope) ((l rope?)
                             (r rope?)
                             (len natural-number/c)
                             (depth natural-number/c))]
 
 
 [string->rope (string? . -> . rope?)]
 [special->rope ((not/c string?) . -> . rope?)]
 
 [rope-append (rope? rope? . -> . rope?)]
 [rope-append* (() #:rest (listof rope?) . ->* . rope?)]
 [rope-has-special? (rope? . -> . boolean?)]
 
 [rope-length (rope? . -> . natural-number/c)]
 [rope-ref (rope? natural-number/c . -> . any)]
 [subrope (case->
           (rope? natural-number/c natural-number/c . -> . rope?)
           (rope? natural-number/c . -> . rope?))]
 
 [rope=? (rope? rope? . -> . boolean?)]
 
 [rope->string (rope? . -> . string?)]
 [rope->vector (rope? . -> . vector?)]
 [vector->rope (vector? . -> . rope?)]
 [input-port->rope (input-port? (any/c . -> . any) . -> . rope?)]
 
 [rope-for-each ((any/c . -> . any) rope? . -> . any)]
 [rope-fold ((any/c any/c . -> . any) any/c rope? . -> . any)]
 [rope-fold/leaves ((rope? any/c . -> . any) any/c rope? . -> . any)]
 
 [open-input-rope (rope? . -> . input-port?)]
 
 [rope-balance (rope? . -> . rope?)]
 [rope-depth (rope? . -> . natural-number/c)]
 [rope-node-count (rope? . -> . natural-number/c)]))