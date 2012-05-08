(library (monad readerT)
         (export readerT
                 unit-readerT
                 bind-readerT
                 bind-reader
                 lift-readerT
                 run-reader
                 ask-readerT
                 local-reader)
         (import (chezscheme)
                 (monad core))

(define unit-readerT
  (lambda (unit bind)
    (lambda (a)
      (lambda (e)
        (unit a)))))

(define bind-readerT
  (lambda (unit bind)
    (lambda (m f)
      (lambda (e)
        (bind (m e)
           (lambda (a)
             ((f a) e)))))))

(define bind-reader
  (lambda (m f)
    (lambda (e)
      ((f (m e)) e))))

(define lift-readerT
  (lambda (unit bind)
    (lambda (m)
      (lambda (e)
        m))))

(define run-reader
  (lambda (m e)
    (m e)))

(define ask-readerT
  (withM (baseM)
    (lambda (e)
      (unit e))))

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