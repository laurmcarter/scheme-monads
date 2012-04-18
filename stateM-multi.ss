(library (monad stateM-multi)
         (export get-state-n
                 put-state-n
                 mod-state-n
                 push-state-n
                 pop-state-n
                 lookup-state-n
                 empty-env-n)
         (import (chezscheme)
                 (monad core)
                 (monad stateM)
                 (monad aux))

;; states reference by list index
;; 

(define get-state-n
  (withM stateM
    (lambda (n)
      (lambda ()
        (doM (e <- (get-state))
             (unit (list-ref e n)))))))

(define put-state-n
  (withM stateM
    (lambda (n)
      (lambda (s^)
        ((mod-state-n n)
         (lambda (s) s^))))))

(define mod-state-n
  (lambda (n)
    (lambda (f)
      (mod-state (mod-list-n n f)))))

(define push-state-n
  (lambda (n)
    (lambda (s^)
      ((mod-state-n n)
       (lambda (s)
         (cons s^ s))))))

(define pop-state-n
  (lambda (n)
    (lambda ()
      (doM-exp bind-state
        (s <- ((get-state-n n)))
        ((mod-state-n n) cdr)
        (unit-state (car s))))))

(define lookup-state-n
  (lambda (n)
    (lambda (x)
      (lookup-state (get-state-n n)))))

;; modify list-ref n of ls with function f
(define mod-list-n
  (lambda (n f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        ((zero? n) (cons (f (car ls)) (cdr ls)))
        (else (cons (car ls)
                    ((mod-list-n (sub1 n) f)
                     (cdr ls))))))))

;; constructs a list of n empty lists
(define empty-env-n
  (lambda (n)
    (lambda ()
      (let loop ((n n))
        (cond
          ((zero? n) '())
          (else (cons '() (loop (sub1 n)))))))))

)