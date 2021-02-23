;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design a function that consumes two images and produces true if the first is larger than the second.

;; (Image, Image) -> Boolean
;; Determines if the first image provided is larger than the second in terms of area
(require 2htdp/image)

(check-expect (has-larger-area?
               (rectangle 10 10 "solid" "red")
               (rectangle 20 20 "solid" "red"))
              false)

(check-expect (has-larger-area?
               (rectangle 20 20 "solid" "red")
               (rectangle 10 20 "solid" "red"))
              true)

; (define (has-larger-area? img1 img2) false) ;stub
;(define (has-larger-area? img1 img2) ; template
;  (... img1)
;  (... img2))

(define (has-larger-area? img1 img2)
  (>(* (image-height img1) (image-width img1))
    (* (image-height img2) (image-width img2))
    ))



;; Self assesment
;; [G]ood | [F]air | [P]oor

;; 1. Is the program "commit ready"
;; Yes, but the parenthesis in the signature's arguments is unecessary.
;; Also, the template's usage of its parameters is wrong.
;; [F]

;; 2. Is the design complete?
;; Yes, all recipe element present.
;; [G]

;; 3. Does the design have high internal quality?
;; Yes, except there is an insufficient amount of tests. All coverage, but edge cases
;; are present when one image's height is large but its width is small.
;; [F]

;; 4. Does the design satisfy the problem requirements?
;; Yes.
;; [G]
