(library (monad maybeM)
         (export maybeM
                 unit-maybe
                 bind-maybe
                 mzero-maybe)
         (import (chezscheme)
                 (monad core)
                 (monad match)
                 (monad aux))

(define unit-maybe
  (lambda (a)
    `(Just ,a)))

(define bind-maybe
  (lambda (m f)
    (match m
      ((Just ,a) (f a))
      ((Nothing) '(Nothing)))))

(define mzero-maybe
  (lambda ()
    '(Nothing)))

(define-monad maybeM
  unit-maybe
  bind-maybe
  mzero-maybe
  mplus-err
  lift-err)

)
