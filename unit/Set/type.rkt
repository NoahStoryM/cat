#lang racket/base

(require racket/case
         racket/match
         racket/list)

(provide type?
         unsafe-type= type=
         normalize-type
         unsafe-get-arity unsafe-get-coarity
         get-arity get-coarity)

(define natural? exact-nonnegative-integer?)

(define (atom? v)
  (and (symbol? v)
       (case/eq v
         [(× + → ←) #f]
         [else #t])))

(define (type? v)
  (match v
    [(or `(× ,tp* ...) `(+ ,tp* ...)) (andmap type? tp*)]
    [(or `(→ ,s ,t) `(← ,t ,s)) (and (type? s) (type? t))]
    [_ (or (atom? v) (natural? v))]))

(define (unsafe-type= tp1 . tp*) (andmap (λ (tp2) (equal? tp1 tp2)) tp*))
(define (type= tp . tp*) (apply unsafe-type= (map normalize-type (cons tp tp*))))

(define (flatten-ops op tp*)
  (append*
   (for/list ([tp (in-list tp*)])
     (match tp
       [`(,(== op eq?) ,tp* ...) tp*]
       [_ (list tp)]))))

(define (process-add tp*)
  (let* ([tp* (map normalize-type tp*)]
         [tp* (flatten-ops '+ tp*)]
         [tp* (remove* '((+)) tp*)])
    (cond
      [(null? tp*) '(+)]
      [(null? (cdr tp*)) (car tp*)]
      [else `(+ . ,tp*)])))

(define (process-mul tp*)
  (let* ([tp* (map normalize-type tp*)]
         [tp* (flatten-ops '× tp*)]
         [tp* (if (member '(+) tp*)
                  '((+))
                  (remove* '((×)) tp*))])
    (cond
      [(null? tp*) '(×)]
      [(null? (cdr tp*)) (car tp*)]
      [else
       (match/values (splitf-at tp* (match-λ [`(+ ,_ ...) #f] [_ #t]))
         [(_ '()) `(× . ,tp*)]
         [(before `((+ ,term* ...) . ,after))
          (process-add
           (for/list ([term (in-list term*)])
             `(× ,@before ,term . ,after)))])])))

(define normalize-type
  (match-λ
    [(? atom? tp) tp]
    [(? natural? tp)
     (case/eqv tp
       [(0) '(+)]
       [(1) '(×)]
       [else (cons '+ (make-list tp '(×)))])]
    [`(+ ,tp* ...) (process-add tp*)]
    [`(× ,tp* ...) (process-mul tp*)]
    [(or `(→ ,s ,t) `(← ,t ,s))
     `(→ ,(normalize-type s) ,(normalize-type t))]))

(define (unsafe-get-arity tp n)
  (let ([tp
         (match tp
           [`(+ ,tp* ...)
            (define l (length tp*))
            (unless (< n l)
              (raise-range-error 'get-arity "tag" "" n 0 (sub1 l)))
            (list-ref tp* n)]
           [_
            (unless (zero? n)
              (raise-range-error 'get-arity "tag" "" n 0 0))
            tp])])
    (match tp
      [`(× ,tp* ...) (length tp*)]
      [_ 1])))
(define (get-arity tp n) (unsafe-get-arity (normalize-type tp) n))

(define (unsafe-get-coarity tp)
  (match tp
    [`(+ ,tp* ...) (length tp*)]
    [_ 1]))
(define (get-coarity tp) (unsafe-get-coarity (normalize-type tp)))
