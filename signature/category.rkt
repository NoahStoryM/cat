#lang racket/signature

(require racket/contract/base)

(contracted
 [dom (-> morphism? morphism?)]
 [cod (-> morphism? morphism?)]
 [∘ (-> morphism? morphism? ... morphism?)]
 [⨾ (-> morphism? morphism? ... morphism?)]
 [morphism= (-> morphism? morphism? ... boolean?)]
 [morphism? (-> any/c boolean?)]
 [object? (-> morphism? boolean?)])
