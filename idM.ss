(library (monad idM)
         (export idM i-unit i-bind i-map)
         (import (chezscheme)
                 (monad core))

;; The most useful monad evar! aka let*

(define i-unit
  (lambda (a) a))

(define i-bind
  (lambda (m f) (f m)))

(define i-map (mapM i-unit i-bind))

(define-monad idM i-unit i-bind mzero-err mplus-err lift-err)

)