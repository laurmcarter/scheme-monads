(library (monad stateM)
         (export stateM
                 unit-state
                 bind-state
                 lookup-state
                 run-state
                 eval-state
                 exec-state
                 get-state
                 put-state
                 mod-state
                 push-state
                 pop-state)
         (import (chezscheme)
                 (monad core))

(define unit-state
  (lambda (a)
    (lambda (s)
      `(,a . ,s))))

(define bind-state
  (lambda (m f)
    (lambda (s)
      (let ((p (m s)))
        (let ((a^ (car p))
              (s^ (cdr p)))
          (let ((m^ (f a^)))
            (m^ s^)))))))

(define lookup-state
  (lambda (get)
    (lambda (x)
      (doM-exp bind-state
        (env <- (get))
        (x^ == (assq x env))
        (unit-state (if x^ (cdr x^) x))))))

(define run-state
  (lambda (m s)
    (m s)))

(define eval-state
  (lambda (m s)
    (car (run-state m s))))

(define exec-state
  (lambda (m s)
    (cdr (run-state m s))))

(define get-state
  (lambda ()
    (lambda (s)
      `(,s . ,s))))

(define put-state
  (lambda (s^)
    (mod-state (lambda (s) s^))))

(define mod-state
  (lambda (f)
    (lambda (s)
      (let ((s^ (f s)))
        `(_ . ,s^)))))

(define push-state
  (lambda (s^)
    (mod-state
     (lambda (s)
       (cons s^ s)))))

(define pop-state
  (lambda ()
    (doM-exp bind-state
      (s <- (get-state))
      (mod-state cdr)
      (unit-state (car s)))))

(define-monad stateM
  unit-state
  bind-state
  mzero-err
  mplus-err
  lift-err)

)