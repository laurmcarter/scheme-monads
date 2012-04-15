(library (monad stateM)
         (export stateM
                 unit-state
                 bind-state
                 map-state
                 lookup-state
                 run-state
                 eval-state
                 exec-state
                 get-state
                 put-state
                 mod-state
                 push-state
                 pop-state
                 prt-state
                 prt-state+e)
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

(define map-state (mapM unit-state bind-state))

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
       (append s^ s)))))

(define pop-state
  (lambda ()
    (doM-exp bind-state
      (s <- (get-state))
      (mod-state cdr)
      (unit-state (car s)))))

; prints args sequentially
(define prt-state
  (lambda e*
    (let loop ((e* e*))
      (cond
        ((null? e*) (printf "\n"))
        (else (begin
                (printf "~a " (car e*))
                (loop (cdr e*))))))
    (unit-state '_)))

; prints args sequentially, then prints current state
(define prt-state+e
  (lambda e*
    (lambda (s)
      ((apply prt-state `(,@e* ":" ,s)) s))))

(define-monad stateM
  unit-state
  bind-state
  mzero-err
  mplus-err
  lift-err)

)