(module rope mzscheme
  (require (lib "etc.ss")
           (lib "plt-match.ss")
           (lib "port.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (only (lib "13.ss" "srfi") string-fold)
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
  (define-struct (rope:concat rope) (l r len))
  
  ;; A leaf-node is considered to be a string-node or special-node.
  
  

  
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
    (rope-balance
     (let loop ([i 0])
       (cond
         [(< (+ i cutoff-before-concat-node-use)
             (string-length a-str))
          (rope-append
           (make-rope:string
            (immutable-substring
             a-str i (+ i cutoff-before-concat-node-use)))
           (loop (+ i cutoff-before-concat-node-use)))]
         [else
          (make-rope:string
           (immutable-substring a-str i))]))))
  
  (define special->rope make-rope:special)
  
  ;; rope-length: rope -> number
  ;; Returns the length of a rope
  (define (rope-length a-rope)
    (match a-rope
      [(struct rope:string (s))
       (string-length s)]
      [(struct rope:special (s))
       1]
      [(struct rope:concat (l r len))
       len]))
  
  ;; rope-has-special? rope -> boolean
  ;; Returns true if the rope has a special.
  (define (rope-has-special? a-rope)
    (match a-rope
      [(struct rope:string (s)) #f]
      [(struct rope:special (s)) #t]
      [(struct rope:concat (l r len))
       (or (rope-has-special? l)
           (rope-has-special? r))]))
  
  
  ;; rope-append: rope rope -> rope
  ;; Puts two ropes together.
  (define (rope-append rope-1 rope-2)
    (local ((define l1 (rope-length rope-1))
            (define l2 (rope-length rope-2))
            (define (make-default-concat r1 r2)
              (make-rope:concat r1 r2 (+ (rope-length r1)
                                         (rope-length r2)))))
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
                        len))
               (struct rope:string (s2)))
         (cond
           [(below-flat-collapsing-cutoff? s1 s2)
            (make-rope:concat
             left-rope
             (make-rope:string (immutable-string-append s1 s2))
             (+ l1 l2))]
           [else
            (make-default-concat rope-1 rope-2)])]
        
        [else
         (make-default-concat rope-1 rope-2)])))
  
  
  
  ;; rope-ref: rope number -> character
  ;; Gets the nth character of a-rope.
  (define (rope-ref a-rope index)
    (match a-rope
      [(struct rope:string (s))
       (string-ref s index)]
      [(struct rope:special (s))
       s]
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
  (define subrope
    (local ((define (subrope a-rope start end)
              (match a-rope
                [(struct rope:string (s))
                 (make-rope:string
                  (immutable-substring s start end))]
                
                [(struct rope:special (s))
                 a-rope]
                
                [(struct rope:concat (rope-1 rope-2 len))
                 (local
                     ((define length-of-rope-1 (rope-length rope-1))
                      (define left
                        (cond
                          [(and (<= start 0)
                                (<= length-of-rope-1 end))
                           rope-1]
                          [(<= length-of-rope-1 start)
                           (make-rope:string "")]
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
                           (make-rope:string "")]
                          [else
                           (subrope rope-2
                                    (max 0 (- start length-of-rope-1))
                                    (max 0 (- end
                                              length-of-rope-1)))])))
                   (rope-append left right))]))
            
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
  
  
  
  ;; rope->string: rope -> string
  ;; Gets a string from the rope.
  (define (rope->string a-rope)
    (match a-rope
      [(struct rope:string (s))
       s]
      [(struct rope:special (s))
       (error 'rope->string "rope contains special ~s" s)]
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
      [(struct rope:string (s))
       (string-fold f acc s)]
      [(struct rope:special (s))
       (f s acc)]
      [(struct rope:concat (l r len))
       (rope-fold f (rope-fold f acc l) r)]))
  
  
  ;; rope-fold/leaves: (string/special X -> X) X rope -> X
  (define (rope-fold/leaves f acc a-rope)
    (match a-rope
      [(struct rope:string (s))
       (f s acc)]
      [(struct rope:special (s))
       (f s acc)]
      [(struct rope:concat (l r len))
       (rope-fold/leaves f (rope-fold/leaves f acc l) r)]))
  
  
  ;; open-input-rope: rope -> input-port
  ;; Opens an input port using the characters in the rope.
  (define (open-input-rope a-rope)
    (match a-rope
      [(struct rope:string (s))
       (open-input-string s)]
      [(struct rope:special (s))
       (let-values ([(inp outp) (make-pipe-with-specials)])
         (write-special s outp)
         (close-output-port outp)
         inp)]
      [(struct rope:concat (l r len))
       (input-port-append
        #t (open-input-rope l) (open-input-rope r))]))
  
  
  ;; rope-balance: rope -> rope
  ;; A fast-and-loose adaptation of the balancing algorithm described
  ;; in the paper.
  (define (rope-balance a-rope)
    (local ((define (add-leaf-to-forest a-leaf a-forest)
              (local ((define leaf-node
                        (cond [(string? a-leaf)
                               (make-rope:string a-leaf)]
                              [else
                               (make-rope:special a-leaf)])))
                (cond
                  [(empty? a-forest)
                   (list leaf-node)]
                  [(< (rope-length leaf-node)
                      (rope-length (first a-forest)))
                   (cons leaf-node a-forest)]
                  [else
                   (local
                       ((define partial-forest
                          (merge-smaller-children
                           a-forest
                           (rope-length leaf-node))))
                     (restore-forest-order
                      (cons (rope-append (first partial-forest)
                                         leaf-node)
                            (rest partial-forest))))])))
            
            (define (merge-smaller-children a-forest n)
              (cond
                [(empty? (rest a-forest))
                 a-forest]
                [(<= (rope-length (first a-forest)) n)
                 a-forest]
                [else
                 (merge-smaller-children
                  (cons (rope-append (second a-forest) (first a-forest))
                        (rest (rest a-forest)))
                  n)]))
            
            (define (restore-forest-order a-forest)
              (cond
                [(empty? (rest a-forest))
                 a-forest]
                [(>= (rope-length (first a-forest))
                     (rope-length (second a-forest)))
                 (restore-forest-order
                  (cons (rope-append (second a-forest) (first a-forest))
                        (rest (rest a-forest))))]
                [else
                 a-forest]))
            
            (define (concatenate-forest a-forest)
              (cond
                [(empty? (rest a-forest))
                 (first a-forest)]
                [else
                 (concatenate-forest
                  (cons (rope-append (second a-forest) (first a-forest))
                        (rest (rest a-forest))))])))
      (concatenate-forest
       (rope-fold/leaves add-leaf-to-forest '() a-rope))))
  
  ;; rope-depth: rope -> natural-number
  (define (rope-depth a-rope)
    (match a-rope
      [(struct rope:string (s))
       0]
      [(struct rope:special (s))
       0]
      [(struct rope:concat (l r len))
       (max (add1 (rope-depth l))
            (add1 (rope-depth r)))]))
  
  
  ;; rope-node-count: rope -> natural-number
  ;; Counts how many nodes (both leaves and concat nodes) are in the rope.
  ;; Just for debugging.
  (define (rope-node-count a-rope)
    (match a-rope
      [(struct rope:string (s))
       1]
      [(struct rope:special (s))
       1]
      [(struct rope:concat (l r len))
       (add1 (+ (rope-node-count l)
                (rope-node-count r)))]))
  
  
  
  (provide current-optimize-flat-ropes)
  (provide/contract
   [rope? (any/c . -> . boolean?)]
   [rope-has-special? (rope? . -> . boolean?)]
   [rope-append (rope? rope? . -> . rope?)]
   [rope-length (rope? . -> . natural-number/c)]
   [rope-ref (rope? natural-number/c . -> . any)]
   [subrope (case->
             (rope? natural-number/c natural-number/c . -> . rope?)
             (rope? natural-number/c . -> . rope?))]
   
   [rope->string (rope? . -> . string?)]
   [string->rope (string? . -> . rope?)]
   [special->rope ((not/c string?) . -> . rope?)]
   
   [rope-for-each ((any/c . -> . any) rope? . -> . any)]
   [rope-fold ((any/c any/c . -> . any) any/c rope? . -> . any)]
   [rope-fold/leaves ((any/c any/c . -> . any) any/c rope? . -> . any)]
   
   [rope-balance (rope? . -> . rope?)]
   [rope-depth (rope? . -> . natural-number/c)]
   [rope-node-count (rope? . -> . natural-number/c)]
   
   [open-input-rope (rope? . -> . input-port?)]))