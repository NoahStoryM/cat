#lang racket/base

(require racket/contract/base
         racket/unit)

(provide category^)

(define-signature category^
  ((contracted
    [dom (-> morphism? morphism?)]
    [cod (-> morphism? morphism?)]
    [∘ (-> morphism? morphism? ... morphism?)]
    [⨾ (-> morphism? morphism? ... morphism?)]
    [morphism= (-> morphism? morphism? ... boolean?)]
    [morphism? (-> any/c boolean?)]
    [object? (-> morphism? boolean?)])))

