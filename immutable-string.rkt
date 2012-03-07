#lang racket/base
(require racket/contract)

;; immutable-string-append: string string -> immutable-string
;; Appends two strings together, ensuring the result is an
;; immutable string.
(define (immutable-string-append s1 s2)
  (cond
   [(= (string-length s1) 0)
    (string->immutable-string s2)]
   [(= (string-length s2) 0)
    (string->immutable-string s1)]
   [else
    (string->immutable-string (string-append s1 s2))]))


;; immutable-substring: string number number -> immutable-string
;; Gets the substring of a-str, ensuring the result is an
;; immutable string.
(define immutable-substring
  (case-lambda
    [(a-str start)
     (immutable-substring a-str start (string-length a-str))]
    [(a-str start end)
     (cond
      [(and (= 0 start) (= end (string-length a-str)))
       (string->immutable-string a-str)]
      [else
       (string->immutable-string
        (substring a-str start end))])]))


(define immutable-string/c (and/c immutable? string?))

(provide/contract
 [immutable-string-append
  (string? string? . -> . immutable-string/c)]
 [immutable-substring
  (case-> (string? natural-number/c
                   . -> . immutable-string/c)
          (string? natural-number/c natural-number/c
                   . -> . immutable-string/c))]
 [immutable-string/c contract?])