(module info (lib "infotab.ss" "setup")
  (define name "rope")
  (define blurb '("Ropes for fast string concatenation and subsequencing"))
  (define release-notes '((p "Version 3.0: major API changes.")
                          (ul
                           (li "rope-fold/leaves does not automatically unbox the leaf structure.")
                           (li "added rope-depth, rope-append*, rope=?, rope->vector, vector->rope functions.")
                           (li "rope:concat has an additional field 'depth'.")
                           (li "rope-append automatically rebalances a rope if it is too deep."))))
  
  (define categories '(datastructures))
  (define homepage "http://hashcollision.org/")
  (define can-be-loaded-with 'all)
  (define version "3.0")
  (define compile-omit-paths (list "test"))
  (define doc.txt "doc.txt")
  (define primary-file "rope.ss"))

