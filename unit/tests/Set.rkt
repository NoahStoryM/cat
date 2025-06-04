#lang racket/base

(require "../Set.rkt"
         "../Set/type.rkt"
         "../Set/function.rkt"
         "../../signature/category.rkt"
         "../../signature/product.rkt"
         "../../signature/coproduct.rkt"
         racket/match
         racket/unit
         rackunit
         (except-in variant tag))

(displayln 'Start)

(define-syntax-rule (check-variant= e1 e2)
  (check-equal?
   (let*-variant ([(#:tag [n 0] . v1) e1]) (cons v1 n))
   (let*-variant ([(#:tag [n 0] . v2) e2]) (cons v2 n))))

(define-values/invoke-unit Set@
  (import)
  (export category^ product^ coproduct^))


(test-case "Type tests"
  (check-true (type= '(+ (+ a b) (+ c d)) '(+ a b c d)))
  (check-true (type= '(× a (+ b c)) '(+ (× a b) (× a c))))
  (check-true (type= '(× (+ b c) a) '(+ (× b a) (× c a))))
  (check-true (type= '(× (+ a b) (+ c d)) '(+ (× a c) (× a d) (× b c) (× b d))))
  (check-true (type= '(× (+ a b) y (+ c d)) '(+ (× a y c) (× a y d) (× b y c) (× b y d))))
  (check-true (type= '(+ a 0 b) '(+ a b)))
  (check-true (type= '(× a 1 b) '(× a b)))
  (check-true (type= 0 '(+) '(× 0) '(× a 0) '(× 0 a) '(× a 0 b)))
  (check-true (type= 1 '(×)))
  (check-true (type= 2 '(+ (×) (×))))
  (check-true (type= 5 '(+ 3 2) '(+ 2 3)))
  (check-true (type= 6 '(× 2 3) '(× 3 2))))

(define a (ann : (+ Any 1)))
(define b (ann : (+ (× Any Any) 1)))
(define c (ann : (× Any Any)))
(define d (ann : Any))

(define f
  (ann
   (λ (#:tag [n 0] . a*)
     (match* (a* n)
       [((list a) 0) (values a a)]
       [((list  ) 1) (variant #:tag 1)]))
   (→ (+ Any 1) (+ (× Any Any) 1))))

(define g
  (ann
   (λ (#:tag [n 0] . a*)
     (match* (a* n)
       [((list a0 a1) 0) (values a0 a1)]
       [((list      ) 1) (values  0  0)]))
   (→ (+ (× Any Any) 1) (× Any Any))))

(define h
  (ann
   (λ (#:tag [n 0] . a*)
     (match* (a* n)
       [((list a0 a1) 0) a0]))
   (→ (× Any Any) Any)))

(test-case "Variant tests"
  (check-pred object? a)
  (check-pred object? b)
  (check-pred object? c)
  (check-pred object? d)
  (check-variant= (f 'a #:tag 0) (values 'a 'a))
  (check-variant= (f 'a) (variant 'a 'a #:tag 0))
  (check-variant= (f #:tag 1) (variant #:tag 1))
  (check-exn exn:fail:contract? (λ () (∘ f h))))

(test-case "Category tests"
  ;; Existence of composition
  (check-true (morphism= b (cod f) (dom g)))
  (check-true (morphism= a (dom (∘ g f)) (dom f)))
  (check-true (morphism= c (cod (∘ g f)) (cod g)))

  ;; Associativity of composition
  (check-true (morphism= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f))))

  ;; Existence of identity arrows
  (check-true (morphism= a (dom a) (cod a)))

  ;; Composition and identity arrows
  (check-true (morphism= f (∘ f (dom f)) (∘ (cod f) f))))

(test-case "Product tests"
  (define a×b×c (∏ a b c))
  (define b×c×d (∏ b c d))
  (define f×g×h (∏ f g h))

  (check-true (morphism= a×b×c (dom f×g×h)))
  (check-true (morphism= b×c×d (cod f×g×h)))

  (check-variant= (f×g×h 1 2 3 4 5) (values 1 1 2 3 4))
  (check-variant= (f×g×h 1 2 3 #:tag 1) (values 1 1 0 0 2))
  (check-variant= (f×g×h 1 2 3 4 #:tag 2) (variant 1 2 3 #:tag 1))
  (check-variant= (f×g×h 1 2 #:tag 3) (variant 0 0 1 #:tag 1)))

(define i
  (ann
   (λ (#:tag [n 0] . a*)
     (match* (a* n)
       [((list a) 0) (values a a)]
       [((list  ) 1) (values 0 0)]))
   (→ (+ Any 1) (× Any Any))))

(define j
  (ann
   (λ (#:tag [n 0] . a*)
     (match* (a* n)
       [((list a) 0) a]
       [((list  ) 1) 0]))
   (→ (+ Any 1) Any)))

(test-case "Pairing tests"
  (define b×c×d (∏ b c d))
  (define f○i○j (○ f i j))

  (check-true (morphism= a (dom f) (dom i) (dom j) (dom f○i○j)))
  (check-true (morphism= b×c×d (cod f○i○j)))

  (check-variant= (f○i○j 9) (values 9 9 9 9 9))
  (check-variant= (f○i○j #:tag 1) (variant 0 0 0 #:tag 1)))

(test-case "Coproduct tests"
  (define a+b+c (∐ a b c))
  (define b+c+d (∐ b c d))
  (define f+g+h (∐ f g h))

  (check-true (morphism= a+b+c (dom f+g+h)))
  (check-true (morphism= b+c+d (cod f+g+h)))

  (check-variant= (f+g+h 1) (values 1 1))
  (check-variant= (f+g+h #:tag 1) (variant #:tag 1))
  (check-variant= (f+g+h 1 2 #:tag 2) (variant 1 2 #:tag 2))
  (check-variant= (f+g+h #:tag 3) (variant 0 0 #:tag 2))
  (check-variant= (f+g+h 1 2 #:tag 4) (variant 1 #:tag 3)))

(define m
  (ann
   (λ (x y) (cons x y))
   (→ (× Any Any) Any)))

(define n
  (ann
   (λ (v) (box v))
   (→ Any Any)))

(test-case "Copairing tests"
  (define a+b+c (∐ a b c))
  (define h□m□n (□ h m n))

  (void)
  ;; (check-true (morphism= d (cod h) (cod m) (cod n) (cod (□ h m n))))
  ;; (check-true (morphism= a+b+c (dom h□m□n)))
  ;; (check-variant= (h□m□n 1 2) 1)
  )
