(library (monad writeM)
         (export writeM
                 eval-write
                 exec-write
                 unit-write
                 bind-write
                 init-write
                 pass-write
                 listen-write
                 tell-write
                 listens-write
                 censor-write
                 empty-write
                 diff-write
                 union-write
                 inters-write
                 set-write)
         (import (chezscheme)
                 (monad core)
                 (monad aux))

(define eval-write
  (lambda (e/acc)
    (car e/acc)))

(define exec-write
  (lambda (e/acc)
    (cdr e/acc)))

(define unit-write
  (lambda (a)
    `(,a . ())))

(define bind-write
  (lambda (m f)
    (let+pair (((a . w) m))
      (let ((m^ (f a)))
        (let+pair (((a^ . w^) m^))
          (let ((ww (append w w^)))
            `(,a^ . ,ww)))))))

(define init-write
  (lambda (a w)
    (doM-exp bind-write
      (tell-write w)
      (unit-write a))))

(define pass-write
  (lambda (m)
    (let+pair ((((a . f) . w) m))
      (let ((w^ (f w)))
        `(,a . ,w^)))))

(define listen-write
  (lambda (m)
    (let+pair (((a . w) m))
      `((,a . ,w) . ,w))))

(define tell-write
  (lambda (w)
    `(_ . ,w)))

(define listens-write
  (lambda (f m)
    (doM-exp bind-write
      ((a . w) <- m)
      (w^ == (f w))
      (unit-write `(,a . ,w^)))))

(define censor-write
  (lambda (f)
    (lambda (m)
      (pass-write
       (doM-exp bind-write
         (a <- m)
         (unit-write `(,a . ,f)))))))

(define empty-write (censor-write (lambda (w) '())))

(define diff-write
  (lambda (w^ m)
    ((censor-write
      (lambda (w)
        (difference w w^)))
     m)))

(define union-write
  (lambda (w^ m)
    ((censor-write
      (lambda (w)
        (union w w^)))
     m)))

(define inters-write
  (lambda (w^ m)
    ((censor-write
      (lambda (w)
        (union w w^)))
     m)))

(define set-write (censor-write make-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((memq (car s1) s2)
       (union (cdr s1) s2))
      (else (cons (car s1)
                  (union (cdr s1) s2))))))

(define intersect
  (lambda (s1 s2)
    (cond
      ((or (null? s1)
           (null? s2))
       '())
      (else
       (let ((a (car s1)))
         (if (memq a s2)
             (cons a (intersect (cdr s1) (remq a s2)))
             (intersect (cdr s1) (remq a s2))))))))

(define difference
  (lambda (s1 s2)
    (cond
      ((null? s2) s1)
      (else (difference (remq (car s2) s1) (cdr s2))))))

(define-monad writeM
  unit-write
  bind-write
  mzero-err
  mplus-err
  lift-err)

)