(library (monad exceptT)
         (export exceptT
                 unit-exceptT
                 bind-exceptT
                 lift-exceptT
                 zero-exceptT
                 try/catch
                 from-just)
         (import (chezscheme)
                 (monad match)
                 (monad core))

;; Maybe monad transformer with failure message

(define unit-exceptT
  (lambda (u)
    (lambda (a)
      (u `(Success ,a)))))

(define bind-exceptT
  (lambda (u b)
    (lambda (m f)
      (b m (lambda (x)
             (match x
               ((Success ,b) (f b))
               ((Exception ,mes) (u `(Exception ,mes)))))))))

(define zero-exceptT
  (lambda (u)
    (lambda (mes)
      (u `(Exception ,mes)))))

(define lift-exceptT
  (lambda (u b)
    (lambda (m)
      (b m (lambda (a)
             (u `(Success ,a)))))))

(define try/catch
  (lambda (m f)
    (match m
      ((Success ,a) a)
      ((Exception ,mes) (f mes)))))

(define from-just
  (lambda (m)
    (match m
      ((Success ,a) a)
      ((Exception ,mes) #f))))

(define-transformer exceptT
  unit-exceptT
  bind-exceptT
  zero-exceptT
  (lambda (u b) mplus-err)
  lift-exceptT)

)