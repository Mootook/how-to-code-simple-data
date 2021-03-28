;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
(define T3 (make-tank (/ WIDTH 2) 0))   ;center no movement

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game empty empty T3)) ;starting Game state (no invaders, no missiles, centered tank)

;; Functions:

;; Game -> Game
;; Begin the game.
;; Start with (main G4)
;; <no tests for main functions>

(define (main g)
  (big-bang g
    (on-tick tick-game)     ;Game -> Game
    (to-draw render-game)   ;Game -> Image
    ;(stop-when ...)     ;Game -> Boolean
    (on-key process-input)))   ;KeyEvent Game -> Game

;; Game -> Game
;; interp. process the tick for Game state (moves tank if dx, descends invaders, ascends missiles)
;; !!!

(check-expect (tick-game G4) G4) ;base case
(check-expect
(define (tick-game g) g) ;stub
;; <use template from Game>

#;
(define (tick-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


;; Game -> Image
;; interp. render the Game in its given state
;; !!!

;(define (render-game g) BACKGROUND) ;stub

(define (render-game g)
  (render-t (game-tank g)))

;; Tank -> Image
;; interp. return the image for the given tank on the BACKGROUND
(check-expect (render-t (make-tank 100 0))
                        (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-t t) BACKGROUND) ;stub
;; <use template from tank>
(define (render-t t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))


;; KeyEvent Game -> Game
;; interp. process keyboard input, moving tank if left or right arrow is pressed, and firing missile if
;;         space is pressed.

(check-expect (process-input G4 "up" ) G4)
(check-expect (process-input G4 "left")
              (make-game (game-invaders G4)
                         (fire "up" (tank-x (game-tank G4)) (game-missiles G4))
                         (move-tank (game-tank G4) "left")))
(check-expect (process-input G4 "right")
              (make-game (game-invaders G4)
                         (fire "right" (tank-x (game-tank G4)) (game-missiles G4))
                         (move-tank (game-tank G4) "right")))
;(define (process-input k g) g) ;stub
;; <use template from Game>

(define (process-input g k)
  (make-game (game-invaders g)
             (fire k (tank-x (game-tank g)) (game-missiles g))
             (move-tank (game-tank g) k)))


;; KeyEvent Number ListOfMissiles -> ListOfMissiles
;; interp. if keyevent is space, add another missile to list of missiles, starting at (x, HEIGHT).
(check-expect (fire "left" 100 empty) empty)
(check-expect (fire " " 100 (list M1))
              (list (make-missile 100 HEIGHT) M1))
(check-expect (fire " " 100 (list M1 M2))
              (list (make-missile 100 HEIGHT) M1 M2))

;(define (fire k x lom) lom) ;stub
;; <use template from Missile>

(define (fire k x lom)
  (cond [(key=? " " k) (cons (make-missile x HEIGHT) lom)]
        [else lom]))

;; Tank KeyEvent -> Tank
;; interp. process keyboard input and move the tank if left or right arrow keys pressed.
(check-expect (move-tank (make-tank 0 0) "up")    (make-tank 0 0))  ;No Movement
(check-expect (move-tank (make-tank 0 0) "left")  (make-tank 0 -1)) ;LEFT
(check-expect (move-tank (make-tank 0 0) "right") (make-tank 0 1))  ;RIGHT

;(define (move-tank k g) g) ;stub
;; <use template from Game>

(define (move-tank t k)
  (cond [(key=? "left" k) (make-tank (tank-x t) -1)]
        [(key=? "right" k) (make-tank (tank-x t) 1)]
        [else t]))