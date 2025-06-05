#lang racket/base

(require "initial.rkt"
         racket/unit)

(provide coproduct^)

(define-signature coproduct^
  extends initial^
  (
   inj #;(-> #:codomain id? natural? ?)
   □   #;(->* () (#:codomain id?) #:rest (listof ?) ?)
   ∐   #;(-> ? ... ?)
   ))
