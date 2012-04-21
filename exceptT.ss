(library (monad exceptT)
         (export exceptT
                 unit-exceptT
                 bind-exceptT
                 bind-except
                 lift-exceptT
                 zero-exceptT
                 try/catch)
         (import (chezscheme)
                 (monad match)
                 (monad core))

;; Maybe monad transformer with failure message
;; Also equivalent to Either monad

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

(define bind-except
  (lambda (m f)
    (match m
      ((Success ,a) (f a))
      ((Exception ,mes) `(Exception ,mes)))))

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

;(define try/catch
;  (lambda (M)
;    (withM M
;      (withM (baseM)
;        (lambda (m f)
;          (bind m
;            (lambda (x)
;              (match x
;                ((Success ,a) (unit a))
;                ((Exception ,mes) (f mes))))))))))

(define-transformer exceptT
  unit-exceptT
  bind-exceptT
  zero-exceptT
  mplusT-err
  lift-exceptT)

)