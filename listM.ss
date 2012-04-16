(library (monad listM)
         (export listM
                 unit-list
                 bind-list
                 mzero-list
                 mplus-list
                 guard-list)
         (import (chezscheme)
                 (monad core)
                 (monad aux))

(define unit-list
  (lambda (a)
    `(,a)))

(define bind-list
  (lambda (m f)
    (mapcan f m)))

(define mapcan
  (lambda (f ls)
    (if (null? ls) '()
        (let+pair (((a . d) ls))
          (mplus-list (f a) (mapcan f d))))))

(define mzero-list
  (lambda ()
    '()))

(define mplus-list
  (lambda (m1 m2)
    (append m1 m2)))

(define guard-list
  (lambda (t)
    (if t (unit-list '_) (mzero-list))))

(define-monad listM
  unit-list
  bind-list
  mzero-list
  mplus-list
  lift-err)

)