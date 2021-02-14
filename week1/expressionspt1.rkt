#lang htdp/bsl

(require 2htdp/image)
; !racket %
(sqrt (+ (sqr 3)  (sqr 4)))
; 5


(circle 10 "solid" "red")

; Declaring constants
(define WIDTH 400)
(define HEIGHT 600)


(define (bulb c)
  (circle 40 "solid" c))

(bulb "purple")


