#lang racket/signature

inj #;(-> #:codomain id? natural? ?)
∐   #;(-> ? ... ?)
□   #;(->* () (#:codomain id?) #:rest (listof ?) ?)
