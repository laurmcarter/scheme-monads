(library (monad aux)
         (export mod-in-list
                 letp
                 extend
                 extend*
                 make-set)
         (import (chezscheme))

; mod-in-list takes a tag, a function, and then a
; list of tagged values.
; It modifies the value associated with the tag using the function.
(define mod-in-list
  (lambda (t f)
    (lambda (ls)
      (if (null? ls) '()
          (let ((a (car ls))
                (d (cdr ls)))
            (if (eq? t (car a))
                (cons (cons (car a) (f (cdr a))) d)
                (cons a ((mod-in-list t f) d))))))))

; letp provides match-like pair deconstruction
; NB: letp has let* semantics between bindings.
(define-syntax letp
  (syntax-rules ()
    ((_ () body ...) (let () body ...))
    ((_ ((b e) rest ...) body ...)
     (let-bind b e (letp (rest ...) body ...)))))

(define-syntax let-bind
  (syntax-rules ()
    ((_ (a . r) e . body)
     (let ((t e))
       (letp ((a (car t)))
             (let-bind r (cdr t) . body))))
    ((_ () e . body) (let () . body))
    ((_ a e . body) (let ((a e)) . body))))

(define extend
  (lambda (x)
    (lambda (e)
      (cons x e))))

(define extend*
  (lambda (e^)
    (lambda (e)
      (append e^ e))))

(define make-set
  (lambda (l)
    (cond
      ((null? l) '())
      (else
       (letp (((a . d) l))
         (cons a (make-set (remq a d))))))))

)