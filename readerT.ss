(library (monad readerT)
         (export readerT
                 unit-readerT
                 bind-readerT
                 lift-readerT
                 run-reader
                 ask-reader
                 local-reader)
         (import (chezscheme)
                 (monad core))

(define unit-readerT
  (lambda (u)
    (lambda (a)
      (lambda (e)
        (u a)))))

(define bind-readerT
  (lambda (u b)
    (lambda (m f)
      (lambda (e)
        (b (m e)
           (lambda (a)
             ((f a) e)))))))

(define lift-readerT
  (lambda (u b)
    (lambda (m)
      (lambda (e)
        m))))

(define run-reader
  (lambda (m e)
    (m e)))

(define ask-reader
  (in-transM
   (lambda ()
     (lambda (e) (unit e)))))

(define local-reader
  (lambda (f m)
    (lambda (e)
      (m (f e)))))

(define-transformer readerT
  unit-readerT
  bind-readerT
  mzeroT-err
  mplusT-err
  lift-readerT)

)