#lang info

(define license 'MIT)
(define collection "cat")
(define version "0.0")

(define pkg-desc "A category theory toolkit for Racket.")

(define deps '("base" "variant"))
(define build-deps '("scribble-lib" "rackunit-lib" "racket-doc"))

#;
(define scribblings '(("scribblings/cat.scrbl")))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/tests/).)*$"))
