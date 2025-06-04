#lang racket/base

(require "type.rkt"
         (for-syntax racket/base syntax/parse)
         racket/function
         racket/match
         racket/treelist
         variant)

(provide (struct-out function)
         apply-path
         apply-function
         ann
         function-arity
         function-result-arity
         function-coarity
         function-result-coarity)

(define (apply-path p* #:tag [n 0] . a*)
  ((for/fold ([thk (λ () (apply/variant variant a* #:tag n))])
             ([p (in-treelist p*)])
     (λ () (call-with-variant thk p)))))
(define (apply-function f #:tag [n 0] . a*)
  (match-define (function p s t) f)
  (define arity (unsafe-get-arity s n))
  (unless (= arity (length a*))
    (define name
      (let ([name (object-name (treelist-first p))])
        (if (symbol? name) name 'apply-function)))
    (apply raise-arity-error name arity a*))
  (apply apply-path p #:tag n a*))

(struct function (path source target)
  #:guard (λ (p s t _) (values p (normalize-type s) (normalize-type t)))
  #:property prop:procedure apply-function
  #:transparent)

(define-syntax ann
  (syntax-parser
    #:datum-literals (: → ←)
    [(_ p (~or* (→ s t) (← t s)))
     #'(function (treelist p) `s `t)]
    [(_ : e)
     #'(let ([tp `e]) (function empty-treelist tp tp))]))

(define-values (function-arity function-result-arity)
  (let ()
    (define ((make function->type) f)
      (match (function->type f)
        [`(+ ,tp* ...) (normalize-arity (map unsafe-get-arity tp*))]
        [`(× ,tp* ...) (length tp*)]
        [_ 1]))
    (values (make function-source)
            (make function-target))))

(define (function-coarity f)
  (unsafe-get-coarity (function-source f)))

(define (function-result-coarity f)
  (unsafe-get-coarity (function-target f)))
