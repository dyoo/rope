(module rope mzscheme
  (require (lib "etc.ss")
           (lib "plt-match.ss")
           (lib "port.ss")
           (only (lib "13.ss" "srfi") string-fold)
           (lib "contract.ss"))
  
  ;; Quick and dirty library implementing ropes, closely following
  ;; the description in: 
  ;;
  ;; H. Boehm, R. Atkinson, M. Plass.
  ;; 'Ropes: An Alternative to Strings'
  ;; Software --- Practice and Experience, Vol 25(12), 1315-1330.
  ;; (December 1995)
  ;;
  
  
  ;; TODO: implement rebalancing
  ;; TODO: optimize concatenation of simple characters to the end
  ;; TODO: test cases
  ;; TODO: documentation
  
  
  ;; A rope is either a flat string, or a rope:concat.
  (define-struct rope:concat (l r len) #f)
  
  
  ;; rope?: any -> boolean
  ;; Returns true if a-datum is a rope.
  (define (rope? a-datum)
    (or (string? a-datum)
        (rope:concat? a-datum)))
  
  
  ;; Arbitrary length cutoff until we allocate a new concat node
  ;; TODO: experiment to see what value is good for this.
  (define cutoff 32)
  
  
  ;; rope-concat: rope rope -> rope
  ;; Puts two ropes together.
  (define (rope-concat rope-1 rope-2)
    (local ((define l1 (rope-length rope-1))
            (define l2 (rope-length rope-2))
            
            (define (below-cutoff? s1 s2)
              (< (+ (string-length s1)
                    (string-length s2)) cutoff))
            
            (define (convert-flats-to-immutable a-rope)
              (cond
                [(string? a-rope)
                 (string->immutable-string a-rope)]
                [else a-rope]))
            
            (define (immutable-string-append s1 s2)
              (string->immutable-string (string-append s1 s2))))
      (cond
        [(and (string? rope-1) (string? rope-2)
              (below-cutoff? rope-1 rope-2))
         (immutable-string-append rope-1 rope-2)]
        
        [(and (rope:concat? rope-1)
              (string? (rope:concat-r rope-1))
              (string? rope-2)
              (below-cutoff? (rope:concat-r rope-1) rope-2))
         (make-rope:concat (rope:concat-l rope-1)
                           (immutable-string-append
                            (rope:concat-r rope-1) rope-2)
                           (+ l1 l2))]
        
        [else
         (make-rope:concat (convert-flats-to-immutable rope-1)
                           (convert-flats-to-immutable rope-2)
                           (+ l1 l2))])))
  
  
  ;; rope-length: rope -> number
  ;; Returns the length of a rope
  (define (rope-length a-rope)
    (match a-rope
      [(? string?)
       (string-length a-rope)]
      [(struct rope:concat (l r len))
       len]))
  
  
  ;; rope-ref: rope number -> character
  ;; Gets the nth character of a-rope.
  (define (rope-ref a-rope index)
    (match a-rope
      [(? string?)
       (string-ref a-rope index)]
      [(struct rope:concat (l r len))
       (local ((define l-length (rope-length l)))
         (cond
           [(< index l-length)
            (rope-ref l index)]
           [else
            (rope-ref r (- index l-length))]))]))
  
  
  ;; subrope: rope number number -> rope
  ;; Takes a subsequence of the rope from start,
  ;; up to (but not including) end.
  (define (subrope a-rope start end)
    (match a-rope
      [(? string?)
       (cond
         [(<= start end)
          (substring a-rope
                     (max start 0)
                     (min end (string-length a-rope)))]
         [else
          ""])]
      [(struct rope:concat (rope-1 rope-2 len))
       (local ((define left
                 (cond
                   [(and (<= start 0)
                         (< (rope-length rope-1) end))
                    rope-1]
                   [else
                    (subrope rope-1 start
                             (min end (rope-length rope-1)))]))
               (define right
                 (cond
                   [(and (<= start (rope-length rope-1))
                         (>= (+ start (- end start))
                             (+ (rope-length rope-1)
                                (rope-length rope-2))))
                    rope-2]
                   [else
                    (subrope rope-2 (- start (rope-length rope-1))
                             (- end (rope-length left)))])))
         (rope-concat left right))]))
  
  
  ;; rope->string: rope -> string
  ;; Gets a string from the rope.
  (define (rope->string a-rope)
    (match a-rope
      [(? string?)
       a-rope]
      [(struct rope:concat (l r len))
       (string-append (rope->string l)
                      (rope->string r))]))
  
  ;; rope-for-each: (char -> void) rope -> void
  ;; Iterates a function f across each character in the rope.
  (define (rope-for-each f a-rope)
    (rope-fold (lambda (ch acc) (f ch)) (void) a-rope))
  
  
  ;; fope-fold: (char X -> X) X rope -> X
  ;; Folds a character-consuming accumulator across the characters
  ;; in the rope.
  (define (rope-fold f acc a-rope)
    (match a-rope
      [(? string?)
       (string-fold f acc a-rope)]
      [(struct rope:concat (l r len))
       (rope-fold f (rope-fold f acc l) r)]))
  
  
  ;; open-input-rope: rope -> input-port
  ;; Opens an input port using the characters in the rope.
  (define (open-input-rope a-rope)
    (match a-rope
      [(? string?)
       (open-input-string a-rope)]
      [(struct rope:concat (l r len))
       (input-port-append
        #t (open-input-rope l) (open-input-rope r))]))
  
  
  (provide/contract
   [rope? (any/c . -> . boolean?)]
   [rope-concat (rope? rope? . -> . rope?)]
   [rope-length (rope? . -> . natural-number/c)]
   [rope-ref (rope? natural-number/c . -> . char?)]
   [subrope (rope? natural-number/c natural-number/c . -> . rope?)]
   [rope->string (rope? . -> . string?)]
   [rope-for-each ((char? . -> . any) rope? . -> . any)]
   [rope-fold ((char? any/c . -> . any) any/c rope? . -> . any)]
   [open-input-rope (rope? . -> . input-port?)]))