(library (monad idM)
         (export idM
                 unit-id
                 bind-id)
         (import (chezscheme)
                 (monad core))

(define unit-id
  (lambda (a) a))

(define bind-id
  (lambda (m f) (f m)))

(define-monad idM
  unit-id
  bind-id
  mzero-err
  mplus-err
  lift-err)

)