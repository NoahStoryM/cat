#lang racket/signature

(require racket/contract/base)

(contracted
 [dom (-> ? ?)]
 [cod (-> ? ?)]
 [∘ (-> ? ? ... ?)]
 [⨾ (-> ? ? ... ?)]
 [= (-> any/c any/c ... boolean?)]
 [? (-> any/c boolean?)]
 [id? (-> ? boolean?)])
