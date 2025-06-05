#lang racket/base

(require racket/contract/base
         racket/unit)

(provide category^)

(define-signature category^
  ((contracted
    [dom (-> morphism? object?)]
    [cod (-> morphism? object?)]
    [id (-> object? (and/c morphism? identity?))]
    [src (-> morphism? (and/c morphism? identity?))]
    [tgt (-> morphism? (and/c morphism? identity?))]
    [∘ (-> morphism? morphism? ... morphism?)]
    [⨾ (-> morphism? morphism? ... morphism?)]
    [morphism= (-> morphism? morphism? ... boolean?)]
    [morphism? (-> any/c boolean?)]
    [identity? (-> morphism? boolean?)]
    [object= (-> object? object? ... boolean?)]
    [object? (-> any/c boolean?)])))
