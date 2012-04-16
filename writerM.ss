(library (monad writerM)
         (export writerM
                 eval-writer
                 exec-writer
                 unit-writer
                 bind-writer
                 init-writer
                 pass-writer
                 listen-writer
                 tell-writer
                 listens-writer
                 censor-writer
                 empty-writer
                 diff-writer
                 union-writer
                 inters-writer
                 set-writer)
         (import (chezscheme)
                 (monad core)
                 (monad aux))

(define eval-writer
  (lambda (e/acc)
    (car e/acc)))

(define exec-writer
  (lambda (e/acc)
    (cdr e/acc)))

(define unit-writer
  (lambda (a)
    `(,a . ())))

(define bind-writer
  (lambda (m f)
    (let+pair (((a . w) m))
      (let ((m^ (f a)))
        (let+pair (((a^ . w^) m^))
          (let ((ww (append w w^)))
            `(,a^ . ,ww)))))))

(define init-writer
  (lambda (a w)
    (doM-exp bind-writer
      (tell-writer w)
      (unit-writer a))))

(define pass-writer
  (lambda (m)
    (let+pair ((((a . f) . w) m))
      (let ((w^ (f w)))
        `(,a . ,w^)))))

(define listen-writer
  (lambda (m)
    (let+pair (((a . w) m))
      `((,a . ,w) . ,w))))

(define tell-writer
  (lambda (w)
    `(_ . ,w)))

(define listens-writer
  (lambda (f m)
    (doM-exp bind-writer
      ((a . w) <- m)
      (w^ == (f w))
      (unit-writer `(,a . ,w^)))))

(define censor-writer
  (lambda (f)
    (lambda (m)
      (pass-writer
       (doM-exp bind-writer
         (a <- m)
         (unit-writer `(,a . ,f)))))))

(define empty-writer (censor-writer (lambda (w) '())))

(define diff-writer
  (lambda (w^ m)
    ((censor-writer
      (lambda (w)
        (difference w w^)))
     m)))

(define union-writer
  (lambda (w^ m)
    ((censor-writer
      (lambda (w)
        (union w w^)))
     m)))

(define inters-writer
  (lambda (w^ m)
    ((censor-writer
      (lambda (w)
        (union w w^)))
     m)))

(define set-writer (censor-writer make-set))

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

(define-monad writerM
  unit-writer
  bind-writer
  mzero-err
  mplus-err
  lift-err)

)