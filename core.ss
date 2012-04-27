(library (monad core)
         (export doM doM-exp <- == ><
                 withM
                 printM
                 whenM
                 ifM
                 nopM
                 define-monad
                 define-transformer
                 mzero-err
                 mplus-err
                 lift-err
                 mzeroT-err
                 mplusT-err
                 mapM
                 foldM
                 promoteF)
         (import (chezscheme)
                 (monad aux))

;; NB: Any functions that are provided here may NOT use the withM form,
;;       they should instead be abstracted with unit/bind.
(define-syntax (withM x)
  (syntax-case x ()
    ((k m b b* ...)
     (with-implicit (k unit bind mmap mfold mzero mplus lift mlift baseM thisM M)
       #'(let ((mm m))
           (let ((M (lambda (f) (f mm)))
                 (unit (monad-unit mm))
                 (bind (monad-bind mm))
                 (mzero (monad-zero mm))
                 (mplus (monad-plus mm))
                 (lift (monad-lift mm))
                 (baseM (monad-base mm))
                 (thisM mm))
             (let ((mlift (M promoteF))
                   (mmap (M mapM))
                   (mfold (M foldM)))
               b b* ...)))))))

(define-syntax (printM x)
  (syntax-case x ()
    ((k e e* ...)
     (with-implicit (k unit)
       #'(let loop ((ls (list e e* ...)) (first #t))
           (if (null? ls)
               (begin
                 (printf "\n")
                 (unit '_))
               (begin
                 (unless first (printf " "))
                 (printf "~a" (car ls))
                 (loop (cdr ls) #f))))))))

(define-syntax (whenM x)
  (syntax-case x ()
    ((k t e)
     (with-implicit (k unit)
       #'(if t e (unit '_))))))

(define-syntax (nopM x)
  (syntax-case x ()
    ((k)
     (with-implicit (k unit)
       #'(unit '_)))))

(define-syntax (doM x)
  (syntax-case x (<-)
    ((k e e* ...)
     (with-implicit (k bind)
       #'(doM-exp bind e e* ...)))))

;; do macro, extended with:
;; >< operator for cata like binding
;; == operator for simple aliasing
;; monadic "bind" w/o result (>>)
;; list binding -- Still working on this one
;;                  - currently matches sequentially,
;;                    ideally would match simultaneously
;; doM *explicit*
(define-syntax doM-exp
  (syntax-rules (<- == >< ifM)
    ;; base case
    ((_ b e) e)
    ;; pretty branching
    ((_ (ifM t et et* ...) ef ef* ...)
     (if t (doM et et* ...) (doM ef ef* ...)))
    ;; bind (with pair/list deconstruction)
    ((_ b (v <- e) e* e** ...)
     (b e (lambda (x) (letp ((v x)) (doM-exp b e* e** ...)))))
    ;; alias (with pair/list deconstruction)
    ((_ b (v == e) e* e** ...)
     (letp ((v e)) (doM-exp b e* e** ...)))
    ;; transform and rebind
    ((_ b (v >< f) e* e** ...)
     (b (f v) (lambda (v) (doM-exp b e* e** ...))))
    ;; transform and rebind (with -arity larger than 1)
    ;; NB: especially useful for mapping
    ((_ b (v more ... >< f) e* e** ...)
     (b (f v more ...) (lambda (v) (doM-exp b e* e** ...))))
    ;; bind hukarz
    ((_ b e e* e** ...)
     (b e (lambda (_) (doM-exp b e* e** ...))))))

(define-syntax (ifM x)
  (syntax-violation #f "misplaced aux keyword" x))

(define-syntax (<- x)
  (syntax-violation #f "misplaced aux keyword" x))

(define-syntax (== x)
  (syntax-violation #f "misplaced aux keyword" x))

(define-syntax (>< x)
  (syntax-violation #f "misplaced aux keyword" x))

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
           (make-monad (u unit)
                       (b unit bind)
                       (z unit)
                       (p unit bind)
                       (l unit bind)
                       (lambda () m))))))))

;; mapM maps a potentially effectual function over a list
(define mapM
  (lambda (m)
    (let ((u (monad-unit m))
          (b (monad-bind m)))
      (lambda (f)
        (lambda (e* . more)
          (if (null? more)
              (let mapM1 ((e* e*))
                (cond
                  ((null? e*) (u '()))
                  (else
                   (doM-exp b
                     (e <- (f (car e*)))
                     (r <- (mapM1 (cdr e*)))
                     (u (cons e r))))))
              (let mapM-more ((e* e*) (more more))
                (cond
                  ((or (null? e*) (exists null? more)) (u '()))
                  (else
                   (doM-exp b
                     (e <- (apply f (car e*) (map car more)))
                     (r <- (mapM-more (cdr e*) (map cdr more)))
                     (u (cons e r))))))))))))

(define foldM
  (lambda (m)
    (let ((u (monad-unit m))
          (b (monad-bind m)))
      (lambda (f a)
        (lambda (e* . more)
          (if (null? more)
              (let foldM1 ((a a) (e* e*))
                (cond
                  ((null? e*) (u a))
                  (else
                   (doM-exp b
                     (a <- (f a (car e*)))
                     (foldM1 a (cdr e*))))))
              (let foldM-more ((a a) (e* e*) (more more))
                (cond
                  ((or (null? e*) (exists null? more)) (u a))
                  (else
                   (doM-exp b
                     (a <- (apply f a (car e*) (map car more)))
                     (foldM-more a (cdr e*) (map cdr more))))))))))))

;; promotes a pure function to operate on monadic values
(define promoteF
  (lambda (m)
    (let ((u (monad-unit m))
          (b (monad-bind m)))
      (lambda (f)
        (lambda args
          (doM-exp b
            (args >< ((mapM m) (lambda (a) a)))
            (u (apply f args))))))))

;(define promoteF
;  (lambda (u b)
;    (lambda (f)
;      (lambda args
;        (doM-exp b
;          (args >< ((mapM u b) (lambda (a) a)))
;          (u (apply f args)))))))

(define-record monad (unit bind zero plus lift base))

(define base-err
  (lambda () (errorf 'monad "base monad undefined")))

(define mzero-err
  (lambda () (errorf 'monad "mzero undefined")))

(define mplus-err
  (lambda (m1 m2)
    (errorf 'monad "mplus undefined")))

(define lift-err
  (lambda (m)
    (errorf 'monad "lift undefined")))

(define mzeroT-err
  (lambda (u) mzero-err))

(define mplusT-err
  (lambda (u b) mplus-err))

)