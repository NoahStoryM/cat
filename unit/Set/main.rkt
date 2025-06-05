#lang racket/unit

(require "../../signature/category.rkt"
         "../../signature/product.rkt"
         "../../signature/coproduct.rkt"
         "type.rkt"
         "function.rkt"
         racket/format
         racket/list
         racket/match
         racket/treelist
         variant)

(import)
(export category^ product^ coproduct^)


;; *****************************************************************************
;; Utilities
;; *****************************************************************************

(define (dims->vols dim*)
  (define len (vector-length dim*))
  (define vol* (make-vector len 1))
  (for ([dim (in-vector dim* (sub1 len) 0 -1)]
        [i (in-range (- len 2) -1 -1)])
    (define vol (vector-ref vol* (add1 i)))
    (vector-set! vol* i (* dim vol)))
  vol*)

(define (ravel-ids id* dim*)
  (define len (vector-length dim*))
  (for/sum ([id (in-vector id* (sub1 len) -1 -1)]
            [vol (in-vector (dims->vols dim*))])
    (* id vol)))

(define (unravel-pos pos dim*)
  (define len (vector-length dim*))
  (define id* (make-vector len))
  (for/fold ([pos pos])
            ([vol (in-vector (dims->vols dim*))]
             [i (in-range len)])
    (let-values ([(id pos) (quotient/remainder pos vol)])
      (vector-set! id* i id)
      pos))
  id*)

;; *****************************************************************************
;; Category
;; *****************************************************************************

(define (dom f) (ann : ,(function-source f)))
(define (cod f) (ann : ,(function-target f)))
(define ⨾
  (case-λ
   [(f) f]
   [(f . f*)
    (define s (function-source f))
    (for/fold ([p (function-path f)]
               [t (function-target f)]
               #:result (function p s t))
              ([f (in-list f*)])
      (define s (function-source f))
      (unless (unsafe-type= t s)
        (raise-argument-error '⨾ (~a t) (unquoted-printing-string s)))
      (values (treelist-append p (function-path f))
              (function-target f)))]))
(define (∘ . f*) (apply ⨾ (reverse f*)))
(define (morphism= f1 . f*)
  (match-define (function p1 s1 t1) f1)
  (for/and ([f2 (in-list f*)])
    (match-define (function p2 s2 t2) f2)
    (and (unsafe-type= s1 s2)
         (unsafe-type= t1 t2)
         (= (treelist-length p1)
            (treelist-length p2))
         (for/and ([e1 (in-treelist p1)]
                   [e2 (in-treelist p2)])
           (equal? e1 e2)))))
(define (morphism? v) (function? v))
(define (object? f)
  (match-define (function p s t) f)
  (and (treelist-empty? p) (unsafe-type= s t)))

;; *****************************************************************************
;; Product
;; *****************************************************************************

(define |1| (ann : 1))
(define (1<-* . _) (values))
(define (1← s)
  (if (type= s 1)
      |1|
      (ann 1<-* (← 1 ,s))))

(define (proj #:domain s i)
  (match s
    [`(× ,n* ...)
     (define l (length n*))
     (unless (< i l)
       (raise-range-error 'proj "×" "" i 0 (sub1 l)))
     (define t (list-ref n* i))
     (ann (λ v* (list-ref v* i)) (→ ,s ,t))]
    [`(+ ,_ ...)
     (raise-range-error 'proj "×" "" i 0 -1)]
    [_
     (unless (zero? i)
       (raise-range-error 'proj "×" "" i 0 0))
     (ann : ,s)]))

(define (∏ . f*)
  (if (member |0| f* morphism=)
      |0|                                      ; 0 = 0 × f = f × 0
      (match (remove* (list |1|) f* morphism=) ; f = 1 × f = f × 1
        [(list) |1|]
        [(list f) f]
        [f*
         (define len (length f*))
         (define-values (p* s* t*)
           (for/lists (p* s* t*) ([f (in-list f*)])
             (match-define (function p s t) f)
             (values p s t)))
         (define s (normalize-type `(× . ,s*)))
         (define t (normalize-type `(× . ,t*)))
         (define p
           (if (andmap treelist-empty? p*)
               empty-treelist           ; Identity function
               (treelist
                (λ (#:tag [#;input-tag it 0] . arg*)
                  ;; Check inputs
                  (define arity (unsafe-get-arity s it))
                  (unless (= (length arg*) arity)
                    (apply raise-arity-error '∏ arity arg*))

                  ;; Split inputs
                  (define #;input-tags it*
                    (unravel-pos it
                     (for/vector #:length len ([s (in-list s*)])
                       (get-coarity s))))
                  (define #;component-arguments* comp-args* (make-vector len))
                  (for/fold ([arg* arg*])
                            ([s (in-list s*)]
                             [it (in-vector it*)]
                             [i (in-range len)])
                    (let-values ([(comp-args arg*) (split-at arg* (unsafe-get-arity s it))])
                      (vector-set! comp-args* i comp-args)
                      arg*))

                  ;; Merge outputs
                  (define #;output-tags ot* (make-vector len))
                  (define res*
                    (for/fold ([res* '()])
                              ([p (in-list p*)]
                               [it (in-vector it*)]
                               [comp-args (in-vector comp-args*)]
                               [i (in-range len)])
                      (define-variant (#:tag [#;output-tag ot 0] . #;component-result comp-res)
                        (apply apply-path p #:tag it comp-args))
                      (vector-set! ot* i ot)
                      (append res* comp-res)))
                  (define #;output-tag ot
                    (ravel-ids ot*
                     (for/vector #:length len ([t (in-list t*)])
                       (get-coarity t))))

                  ;; Check outputs
                  (define result-arity (unsafe-get-arity t ot))
                  (unless (= (length res*) result-arity)
                    (apply raise-result-arity-error '∏ result-arity #f res*))

                  (apply/variant variant #:tag ot res*)))))
         (function p s t)])))

(define (○ #:domain [s #f] . f*)
  (if (null? f*)
      (or (and s (1← s)) |1|)
      (let ([s (or (and s (normalize-type s))
                   (function-source (car f*)))])
        (define t*
          (for/list ([f (in-list f*)])
            (unless (unsafe-type= s (function-source f))
              (raise-argument-error
               '○ (~a s)
               (unquoted-printing-string (~a (function-source f)))))
            (function-target f)))
        (define t (normalize-type `(× . ,t*)))
        (define id (ann 1<-* (← 1 ,s)))
        (match (remove* (list id) f* morphism=)
          [(list)
           (unless (equal? t '(×))
             (raise-argument-error
              '○ (~a t)
              (unquoted-printing-string (~a '(×)))))
           id]
          [(list f) f]
          [f*
           (define len (length f*))
           (define p* (map function-path f*))
           (define p
             (treelist
              (λ (#:tag [#;input-tag it 0] . arg*)
                ;; Check inputs
                (define arity (unsafe-get-arity s it))
                (unless (= (length arg*) arity)
                  (apply raise-arity-error '○ arity arg*))

                ;; Apply each function to the same inputs
                (define #;output-tags ot* (make-vector len))
                (define res*
                  (for/fold ([res* '()])
                            ([p (in-list p*)]
                             [i (in-range len)])
                    (define-variant (#:tag [#;output-tag ot 0] . #;component-result comp-res)
                      (apply apply-path p #:tag it arg*))
                    (vector-set! ot* i ot)
                    (append res* comp-res)))
                (define #;output-tag ot
                  (ravel-ids ot*
                   (for/vector #:length len ([t (in-list t*)])
                     (get-coarity t))))

                ;; Check outputs
                (define result-arity (unsafe-get-arity t ot))
                (unless (= (length res*) result-arity)
                  (apply raise-result-arity-error '○ result-arity #f res*))

                (apply/variant variant #:tag ot res*))))
           (function p s t)]))))

;; *****************************************************************************
;; Coproduct
;; *****************************************************************************

(define |0| (ann : 0))
(define 0->* (case-λ))
(define (0→ t)
  (if (type= t 0)
      |0|
      (ann 0->* (→ 0 ,t))))

(define (inj #:codomain t i)
  (match t
    [`(+ ,n* ...)
     (define l (length n*))
     (unless (< i l)
       (raise-range-error 'inj "+" "" i 0 (sub1 l)))
     (define s (list-ref n* i))
     (ann (if (zero? i)
              values
              (λ v* (apply values (tag i) v*)))
          (→ ,s ,t))]
    [_
     (unless (zero? i)
       (raise-range-error 'inj "+" "" i 0 0))
     (ann : ,t)]))

(define (∐ . f*)
  (match (remove* (list |0|) f* morphism=) ; f = 0 + f = f + 0
    [(list) |0|]
    [(list f) f]
    [f*
     (define len (length f*))
     (define-values (p* s* t*)
       (for/lists (p* s* t*) ([f (in-list f*)])
         (match-define (function p s t) f)
         (values p s t)))
     (define s (normalize-type `(+ . ,s*)))
     (define t (normalize-type `(+ . ,t*)))
     (define p
       (if (andmap treelist-empty? p*)
           empty-treelist                 ; Identity function
           (treelist
            (λ (#:tag [#;input-tag it 0] . arg*)
              (define-values (p coarity result-coarity)
                (for/fold ([p #f]
                           [coarity 0]
                           [result-coarity 0])
                          ([f (in-list f*)] #:unless p)
                  (define m (function-coarity f))
                  (define n (function-result-coarity f))
                  (if (> m (- it coarity))
                      (values (function-path f) coarity result-coarity)
                      (values p (+ coarity m) (+ result-coarity n)))))
              (define-variant (#:tag [t 0] . res)
                (apply apply-path p #:tag (- it coarity) arg*))
              (define #;output-tag ot (+ t result-coarity))
              (apply/variant variant #:tag ot res)))))
     (function p s t)]))

(define (□ #:codomain [t #f] . f*)
  (if (null? f*)
      (or (and t (0→ t)) |0|)
      (let ([t (or (and t (normalize-type t))
                   (function-target (car f*)))])
        (define s*
          (for/list ([f (in-list f*)])
            (unless (unsafe-type= t (function-target f))
              (raise-argument-error
               '□ (~a t)
               (unquoted-printing-string (~a (function-target f)))))
            (function-source f)))
        (define s (normalize-type `(× . ,s*)))
        (define id (ann 0->* (→ 0 ,t)))
        (match (remove* (list id) f* morphism=)
          [(list)
           (unless (equal? s '(+))
             (raise-argument-error
              '□ (~a s)
              (unquoted-printing-string (~a '(+)))))
           id]
          [(list f) f]
          [f*
           (define len (length f*))
           (define p* (map function-path f*))
           (define p
             (treelist
              (λ (#:tag [#;input-tag it 0] . arg*)
                (define-values (p coarity)
                  (for/fold ([p #f]
                             [coarity 0])
                            ([f (in-list f*)] #:unless p)
                    (define m (function-coarity f))
                    (if (> m (- it coarity))
                        (values (function-path f) coarity)
                        (values p (+ coarity m)))))
                (apply apply-path p #:tag (- it coarity) arg*))))
           (function p s t)]))))
