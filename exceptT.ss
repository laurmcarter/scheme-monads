(library (monad exceptT)
         (export exceptT
                 unit-exceptT
                 bind-exceptT
                 bind-except
                 lift-exceptT
                 zero-exceptT
                 try/catch)
         (import (chezscheme)
                 (monad core)
                 (monad aux))

;; Maybe monad transformer with failure message
;; Also equivalent to Either monad

(define unit-exceptT
  (lambda (unit bind)
    (lambda (a)
      (unit `(Success . ,a)))))

(define bind-exceptT
  (lambda (unit bind)
    (lambda (m f)
      (bind m (lambda (x)
                (letp (((t . a) x))
                  (case t
                    ((Success) (f a))
                    ((Exception) (unit `(Exception . ,a))))))))))

(define bind-except
  (lambda (m f)
    (letp (((t . a) m))
      (case t
        ((Success) (f a))
        ((Exception) `(Exception . ,a))))))

(define zero-exceptT
  (lambda (unit bind)
    (lambda (mes)
      (unit `(Exception . ,mes)))))

(define lift-exceptT
  (lambda (unit bind)
    (lambda (m)
      (bind m (lambda (a)
                (unit `(Success . ,a)))))))

(define try/catch
  (lambda (m f)
    (letp (((t . a) m))
      (case t
        ((Success) a)
        ((Exception) (f a))))))

(define-transformer exceptT
  unit-exceptT
  bind-exceptT
  zero-exceptT
  mplusT-err
  lift-exceptT)

)