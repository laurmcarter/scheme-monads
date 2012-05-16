(library (monad core)
         (export unit bind mzero mplus lift baseM
                 current-monad
                 doM <- == ><
                 withM
                 printM
                 whenM
                 nopM
                 mapM
                 foldM
                 liftM
                 define-monad
                 define-transformer
                 monad-error
                 mzero-err
                 mplus-err
                 lift-err
                 mzeroT-err
                 mplusT-err)
         (import (chezscheme)
                 (monad aux))

(define-syntax unit (identifier-syntax (monad-unit (current-monad))))
(define-syntax bind (identifier-syntax (monad-bind (current-monad))))
(define-syntax mzero (identifier-syntax (monad-zero (current-monad))))
(define-syntax mplus (identifier-syntax (monad-plus (current-monad))))
(define-syntax lift (identifier-syntax (monad-lift (current-monad))))
(define-syntax baseM (identifier-syntax (monad-unit (current-monad))))

(define-syntax withM
  (syntax-rules ()
    ((_ m b b* ...)
     (parameterize ((current-monad m))
       (begin
         b b* ...)))))

(define monad-error
  (lambda (t)
    (lambda args
      (errorf t "undefined"))))

(define current-monad (make-parameter 'dummy))

(define printM
  (lambda (fst . more)
    (if (null? more)
        (begin
          (pretty-print fst)
          (nopM))
        (begin
          (printf "~a" fst)
          (let loop ((more more))
            (cond
              ((null? more) (begin (newline) (nopM)))
              (else
               (begin
                 (printf " ~a" (car more))
                 (loop (cdr more) )))))))))

(define-syntax whenM
  (syntax-rules ()
    ((_ t e)
     (if t e (nopM)))))

(define nopM
  (lambda ()
    (unit '_)))

;; do macro, extended with:
;; >< operator for cata like binding
;; == operator for simple aliasing
;; monadic bind w/o result (>>)
;; list binding and aliasing
(define-syntax doM
  (syntax-rules (<- == ><)
    ;; base case
    ((_ e) e)
    ;; bind (with pair/list deconstruction)
    ((_ (v <- e) e* e** ...)
     (bind e (lambda (x) (letp ((v x)) (doM e* e** ...)))))
    ;; alias (with pair/list deconstruction)
    ((_ (v == e) e* e** ...)
     (letp ((v e)) (doM e* e** ...)))
    ;; transform and rebind
    ((_ (v >< f) e* e** ...)
     (bind (f v) (lambda (v) (doM e* e** ...))))
    ;; bind hukarz
    ((_ e e* e** ...)
     (bind e (lambda (_) (doM e* e** ...))))))

(define-syntax (<- x)
  (syntax-violation #f "misplaced aux keyword" x))

(define-syntax (== x)
  (syntax-violation #f "misplaced aux keyword" x))

(define-syntax (>< x)
  (syntax-violation #f "misplaced aux keyword" x))

;; mapM maps a potentially effectual function over a list
(define mapM
  (lambda (f)
    (lambda (first . rest)
      (if (null? rest)
          (let mapM1 ((a* first))
            (cond
              ((null? a*) (unit '()))
              (else
               (doM (a <- (f (car a*)))
                    (d <- (mapM1 (cdr a*)))
                    (unit (cons a d))))))
          (let mapM-more ((a* first) (more rest))
            (cond
              ((and (null? a*) (for-all null? more))
               (unit '()))
              ((find null? more) =>
               (lambda (bad)
                 (errorf 'mapM "lists ~a and ~a differ in length" first bad)))
              (else
               (doM (a <- (apply f (car a*) (map car more)))
                    (d <- (mapM-more (cdr a*) (map cdr more)))
                    (unit (cons a d))))))))))

(define foldM
  (lambda (f a)
    (lambda (first . rest)
      (if (null? rest)
          (let foldM1 ((a a) (a* first))
            (cond
              ((null? a*) (unit a))
              (else
               (doM (a <- (f a (car a*)))
                    (foldM1 a (cdr a*))))))
          (let foldM-more ((a a) (a* first) (more rest))
            (cond
              ((and (null? a*) (for-all null? more))
               (unit a))
              ((find null? more) =>
               (lambda (bad)
                 (error 'foldM "lists ~a and ~a differ in length" first bad)))
              (else
               (doM (a <- (apply f a (car a*) (map car more)))
                    (foldM-more a (cdr a*) (map cdr more))))))))))

(define liftM
  (lambda (f)
    (lambda args
      (doM (args >< (lambda (x) x))
           (unit (apply f args))))))

(define-record monad (unit bind zero plus lift base))

(define-syntax define-monad
  (syntax-rules ()
    ((_ id u b z p l)
     (define id
       (make-monad u b z p l base-err)))))

(define-syntax define-transformer
  (syntax-rules ()
    ((_ id u b z p l)
     (define id
       (lambda (m)
         (withM m
           (make-monad
            (u unit bind)
            (b unit bind)
            (z unit bind)
            (p unit bind)
            (l unit bind)
            (lambda () m))))))))

(define mzero-err (monad-error 'mzero))
(define mplus-err (monad-error 'mplus))
(define lift-err (monad-error 'lift))
(define base-err (monad-error 'baseM))

(define mzeroT-err (lambda (u b) mzero-err))
(define mplusT-err (lambda (u b) mplus-err))

)