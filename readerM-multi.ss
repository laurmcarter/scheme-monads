(library (monad readerM-multi)
         (export ask-for
                 local-for
                 with-new-env)
         (import (chezscheme)
                 (monad core)
                 (monad readerM)
                 (monad aux))

; env has form ((t0 . env0) (t1 . env1) ...)
;   where t0, t1, ... are tags for their environments
;   and t0, t, ... are all unique
;   and env0, env1, ... are alists

; accessor for tagged environment
(define ask-for
  (lambda (t)
    (lambda ()
      (lambda (e)
        (cond
          ((assq t e) => cdr)
          (else #f))))))

; local modification of tagged environment
(define local-for
  (lambda (t)
    (lambda (f m)
      (local-reader
       (mod-in-list t f)
       m))))

; introduce new tagged environment in inner computation
(define with-new-env
  (lambda (t/e m)
    (local-reader
     (lambda (e) `(,t/e . ,e))
     m)))

)