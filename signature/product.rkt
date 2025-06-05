#lang racket/base

(require "terminal.rkt"
         racket/unit)

(provide product^)

(define-signature product^
  extends terminal^
  (
   proj #;(-> #:domain id? natural? ?)
   ○    #;(->* () (#:domain id?) #:rest (listof ?) ?)
   ∏    #;(-> ? ... ?)
   ))
