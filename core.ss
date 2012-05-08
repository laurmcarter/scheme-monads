(library (monad core)
         (export unit bind mzero mplus lift baseM
                 doM <- == ><
                 withM
                 printM
                 whenM
                 nopM
                 mapM
                 foldM
                 liftM liftM2 liftM3 liftM4 liftM5
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

(define-syntax withM
  (syntax-rules ()
    ((_ m b b* ...)
     (parameterize ((unit (monad-unit m))
                    (bind (monad-bind m))
                    (mzero (monad-zero m))
                    (mplus (monad-plus m))
                    (lift (monad-lift m))
                    (baseM (monad-base m)))
       (begin
         b b* ...)))))

(define printM
  (lambda (fstr . args)
    (begin
      (pretty-print (apply format fstr args))
      (nopM))))

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
    (lambda (m)
      (doM (x <- m)
           (unit (f x))))))

(define liftM2
  (lambda (f)
    (lambda (m1 m2)
      (doM (x1 <- m1)
           (x2 <- m2)
           (unit (f x1 x2))))))

(define liftM3
  (lambda (f)
    (lambda (m1 m2 m3)
      (doM (x1 <- m1)
           (x2 <- m2)
           (x3 <- m3)
           (unit (f x1 x2 x3))))))

(define liftM4
  (lambda (f)
    (lambda (m1 m2 m3 m4)
      (doM (x1 <- m1)
           (x2 <- m2)
           (x3 <- m3)
           (x4 <- m4)
           (unit (f x1 x2 x3 x4))))))

(define liftM5
  (lambda (f)
    (lambda (m1 m2 m3 m4 m5)
      (doM (x1 <- m1)
           (x2 <- m2)
           (x3 <- m3)
           (x4 <- m4)
           (x5 <- m5)
           (unit (f x1 x2 x3 x4 x5))))))

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
         (let ((unit (monad-unit m))
               (bind (monad-bind m)))
           (make-monad (u unit bind)
                       (b unit bind)
                       (z unit bind)
                       (p unit bind)
                       (l unit bind)
                       (lambda () m))))))))

(define monad-error
  (lambda (t)
    (lambda args
      (errorf t "undefined"))))

(define mzero-err (monad-error 'mzero))
(define mplus-err (monad-error 'mplus))
(define lift-err (monad-error 'lift))
(define base-err (monad-error 'baseM))

(define mzeroT-err (lambda (u b) mzero-err))
(define mplusT-err (lambda (u b) mplus-err))

(define unit (make-parameter (monad-error 'unit)))
(define bind (make-parameter (monad-error 'bind)))
(define mzero (make-parameter mzero-err))
(define mplus (make-parameter mplus-err))
(define lift (make-parameter lift-err))
(define baseM (make-parameter base-err))

)