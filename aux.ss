(library (monad aux)
         (export mod-in-list
                 let+pair
                 extend)
         (import (chezscheme))

; mod-in-list takes a tag, a function, and then a
; list of tagged values.
; It modifies the value associated with the tag
; using the function
(define mod-in-list
  (lambda (t f)
    (lambda (ls)
      (if (null? ls) '()
          (let ((a (car ls))
                (d (cdr ls)))
            (if (eq? t (car a))
                (cons (cons (car a) (f (cdr a))) d)
                (cons a ((mod-in-list t f) d))))))))

; let+pair provides match-like pair deconstruction
(define-syntax let+pair
  (syntax-rules ()
    [(_ () body ...) (let () body ...)]
    [(_ ([b e] rest ...) body ...)
     (let-bind b e (let+pair (rest ...) body ...))]))

(define-syntax let-bind
  (syntax-rules ()
    [(_ (a . r) e . body)
     (let ([t e])
       (let ([a (car t)])
         (let-bind r (cdr t) . body)))]
    [(_ () e . body) (let () . body)]
    [(_ a e . body) (let ([a e]) . body)]))

(define extend
  (lambda (e^)
    (lambda (e)
      (append e^ e))))

)