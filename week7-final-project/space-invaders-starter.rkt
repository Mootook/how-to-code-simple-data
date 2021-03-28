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
(define INVADER-Y-SPEED 1.2)
(define TANK-SPEED 5)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 1)

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

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define BEGIN (make-game empty empty T3)) ;starting Game state (no invaders, no missiles, centered tank)

;; Functions:

;; Game -> Game
;; Begin the game.
;; Start with (main BEGIN)
;; <no tests for main functions>

(define (main g)
  (big-bang g
    (on-tick   tick-game)                  ;Game -> Game
    (to-draw   render-game)                ;Game -> Image
    (stop-when game-over game-over-screen) ;Game -> Boolean
    (on-key    process-input)))            ;KeyEvent Game -> Game

;; Game -> Boolean
;; interp. game over if an invader has landed

(check-expect (game-over BEGIN) false)
(check-expect (game-over (make-game (list (make-invader 100 HEIGHT 1))
                                    (list M1)
                                    T1))
              true)
;(define (game-over s) false) ;stub
;; <use template from Game>
(define (game-over s)
  (cond [(empty? (game-invaders s)) false]
        [else
         (if (landed? (first (game-invaders s)))
             true 
             (game-over (make-game (rest (game-invaders s))
                                   (game-missiles s)
                                   (game-tank s))))]))

;; Game -> Image
;; interp. produce game over screen
(define (game-over-screen s)
  (place-image (text "GAME OVER" 50 "red")
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (render-game s)))
                         
         
;; Invader -> Boolean
;; interp. return true if invader has landed
(check-expect (landed? (make-invader 10 10 1)) false)
(check-expect (landed? (make-invader 10 HEIGHT 1)) true)
;(define (landed? i) false) ;stub

(define (landed? i)
  (>= (invader-y i) HEIGHT))
        


;; Game -> Game
;; interp. process the tick for Game state (moves tank if dx, descends invaders, ascends missiles)
(check-expect (tick-game BEGIN) BEGIN) ;base case

(check-expect (tick-game G3)
              (make-game (tick-invaders (game-invaders G3) (game-missiles G3))
                         (on-screen-lom (tick-missiles (game-missiles G3)))
                         (tick-t (game-tank G3))))
;(define (tick-game s) s) ;stub
;; <use template from Game>
(define (tick-game s)
  (make-game (tick-invaders (game-invaders s) (game-missiles s))
             (on-screen-lom (tick-missiles (game-missiles s)))
             (tick-t (game-tank s))))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; tick process for the list of invaders in the active Game state. Apply downard (angled) movement, check for missile collisions,
;; and randomly spawn new invaders.
(check-expect (tick-invaders empty empty) empty)
(check-expect (tick-invaders (list I1) empty)
              (move-invaders (destroy-invaders (create-invaders (list I1))
                                               empty)))
;(define (tick-invaders loi lom) loi) ;stub
(define (tick-invaders loi lom)
  (move-invaders (destroy-invaders (create-invaders loi)
                                   lom)))

;; ListOfInvaders -> ListOfInvaders
;; interp. randomly spawn new invaders and add to given ListOfInvaders
(check-expect (create-invaders empty)
              (if (spawn? INVADE-RATE)
                  (list (make-invader (random WIDTH) 0 (invader-dir (random 10))))
                  empty))
(check-expect (create-invaders (list I1))
              (if (spawn? INVADE-RATE)
                  (cons (make-invader (random WIDTH) 0 (invader-dir (random 10))) (cons I1 empty))
                  (list I1)))

;(define (create-invaders loi) loi) ;stub

(define (create-invaders loi)
  (if (spawn? INVADE-RATE)
      (cons (make-invader (random WIDTH) 0 (invader-dir (random 10))) loi)
      loi))

;; Integer -> Integer
;; interp. produce a random direction with the given number, can be d or -d
(define (invader-dir d)
  (if (equal? (modulo d 2) 0)
      d
      (- d)))

;; Integer -> Boolean
;; returns true if INVADE-RATE is less than random generated number
;; no tests for random number generation.
;(define (spawn? n) false) ;stub
(define (spawn? n)
  (<= (random 100) n))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; interp. remove any invaders that have been hit by any missile from ListOfMissile
(check-expect (destroy-invaders empty empty) empty)
(check-expect (destroy-invaders (list I1) empty)
              (list I1))
(check-expect (destroy-invaders (list I1) (list (make-missile (invader-x I1) (invader-y I1))))
              empty)
(check-expect (destroy-invaders (list I1 I2)
                                (list (make-missile (invader-x I2) (invader-y I2))))
              (list I1))

;(define (destroy-invaders loi lom) loi) ;stub
(define (destroy-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (destroy? (first loi) lom)
             (destroy-invaders (rest loi) lom)
             (cons (first loi) (destroy-invaders (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; interp. return true if any missiles in ListOfMissiles collides with given invader
(check-expect (destroy? (make-invader 100 100 10) empty) false)
(check-expect (destroy? (make-invader 100 100 10) (list (make-missile 100 100))) true)
(check-expect (destroy? (make-invader 100 100 10) (list (make-missile 10 10) (make-missile 100 100))) true)
;(define (destroy? i lom) false) ;stub

(define (destroy? i lom)
  (cond [(empty? lom) false]
        [else
         (if (collision? i (first lom))
             true
             (destroy? i (rest lom)))]))

;; Invader Missile -> Boolean
;; interp. return true if the given missile and invader collide

(check-expect (collision? (make-invader 100 100 10) (make-missile 100 0)) false)
(check-expect (collision? (make-invader 100 100 10) (make-missile 100 100)) true)

;(define (collision? i m) false) ;stub
(define (collision? i m)
  (and (< (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (< (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))        

;; ListOfInvaders -> ListOfInvaders
;; move the given list invaders down the screen, applying (invader-dx i) to its x-translation.
;; the invader will also bounce of the vertical sides of the screen and maintain velocity in the
;; opposite direction.
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list I1))
              (cons (move-invader I1)
                    empty))
(check-expect (move-invaders (list I1 I2))
              (cons (move-invader I1)
                    (cons (move-invader I2)
                          empty)))
;(define (move-invaders loi) loi) ;stub
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) empty]
        [else
         (... (fn-for-i   (first loi))
              (fn-for-loi (rest  loi)))]))
(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader  (first loi))
               (move-invaders (rest  loi)))]))

;; Invader -> Invader
;; advance the given invader downward, applying dx to its x-translation and checking
;; if it should bounce (reverse dx) off wall.
(check-expect (move-invader I1)                                                    ;applied descent with no change in direction
              (make-invader (+ (invader-x I1) (* INVADER-X-SPEED (invader-dx I1)))
                            (+ (invader-y I1) INVADER-Y-SPEED)
                            (invader-dx I1)))
(check-expect (move-invader (make-invader -1 100 -10))                              ;hits left wall with left trajectory
              (make-invader 0
                            (+ 100 INVADER-Y-SPEED)
                            (- -10)))
(check-expect (move-invader (make-invader (+ 1 WIDTH) 100 10))                           ;hits right wall with right trajectory
              (make-invader WIDTH
                            (+ 100 INVADER-Y-SPEED)
                            (- 10)))
;(define (move-invader i) i) ;stub
;; <use template from Invader>
(define (move-invader i)
  (cond [(> (invader-x i) WIDTH)
         (make-invader WIDTH (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [(< (invader-x i) 0)
         (make-invader 0 (+ (invader-y i) INVADER-Y-SPEED) (- (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))
         

;; ListOfMissiles -> ListOfMissiles
;; filters out missiles below 0 on they (off-screen) and returns only on-screen missiles
;; !!!
(check-expect (on-screen-lom (list M1))
              (list M1))
(check-expect (on-screen-lom (list (make-missile 100 -10)))
              empty)
(check-expect (on-screen-lom (list M1 (make-missile 100 -10)))
              (list M1))
;(define (on-screen-lom lom) lom) ;stub

(define (on-screen-lom lom)
  (cond [(empty? lom) empty]
        [else
         (if (on-screen-m (first lom))
             (cons (first lom) (on-screen-lom (rest lom)))
             (on-screen-lom (rest lom)))]))

;; Missile -> Boolean
;; return true if the missile is still valid (on-screen), deteremined if its y is less than 0, else false.

(check-expect (on-screen-m M1) true)
(check-expect (on-screen-m (make-missile 100 -5)) false)

;(define (on-screen-m m) true) ;stub
;; <use template from Missile>

(define (on-screen-m m)
  (> (missile-y m) 0))

;; ListOfMissiles -> ListOfMissiles
;; tick process for the list of missiles in the active Game state. Apply upward movement, if below 0 on y, filter
;; out of List.
;; !!!
(check-expect (tick-missiles (list M1))
              (cons (tick-missile M1) empty))
(check-expect (tick-missiles (list M1 M2))
              (cons (tick-missile M1) (cons (tick-missile M2) empty)))

;(define (tick-missiles lom) lom) ;stub
#;
(define (fn-for-lom lom)
  (cond [(empty? t) empty]
        [else
         (... (fn-for-m   (first lom))
              (fn-for-lom (rest  lom)))]))

(define (tick-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (tick-missile  (first lom))
               (tick-missiles (rest lom)))]))

;; Missile -> Missile
;; tick process for single missile. Apply upwards movement.
(check-expect (tick-missile M1)
              (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED)))

;(define (tick-missile m) m) ;stub
;; <use template from Missile>
(define (tick-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; tick process for tank, applies movement direction to translation.
(check-expect (tick-t (make-tank 0  0)) (make-tank 0 0))
(check-expect (tick-t (make-tank 0  1)) (make-tank (+ 0 TANK-SPEED)  0))
(check-expect (tick-t (make-tank 100 -1)) (make-tank (- 100 TANK-SPEED)  0))
(check-expect (tick-t (make-tank 0 -1)) (make-tank 0 0))
(check-expect (tick-t (make-tank WIDTH 1)) (make-tank WIDTH 0))

;(define (tick-t t) t) ;stub
;; <use template from Tank>
(define (tick-t t)
  (cond [(equal?  1 (tank-dir t))
         (if (>= (tank-x t) WIDTH)
             (make-tank WIDTH 0)
             (make-tank (+ (tank-x t) TANK-SPEED) 0))]
        [(equal? -1 (tank-dir t))
         (if (<= (tank-x t) 0)
             (make-tank 0 0)
             (make-tank (- (tank-x t) TANK-SPEED) 0))]
        [else t]))

;; Game -> Image
;; interp. render the Game in its given state
(check-expect (render-game BEGIN)
              (render-loi (game-invaders BEGIN)
                          (render-lom (game-missiles BEGIN)
                                      (render-t (game-tank BEGIN)))))
;(define (render-game s) BACKGROUND) ;stub
;; <use template from Game>
(define (render-game s)
  (render-loi (game-invaders s)
              (render-lom (game-missiles s)
                          (render-t (game-tank s)))))


;; ListOfInvaders -> Image
;; render the given list of invaders, placing it on the given img
;(define (render-loi loi img) img) ;stub
(check-expect (render-loi empty (render-lom empty (render-t T1)))
              (render-t T1))
(check-expect (render-loi (list I1) (render-lom empty (render-t T1)))
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (render-lom empty (render-t T1))))
(check-expect (render-loi (list I1 I2) (render-lom (list M1) (render-t T1)))
              (place-image INVADER
                           (invader-x I1)
                           (invader-y I1)
                           (place-image INVADER
                                        (invader-x I2)
                                        (invader-y I2)
                                        (render-lom (list M1) (render-t T1)))))
(define (render-loi loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-loi (rest loi) img))]))


;; ListOfMissiles -> Image
;; render the given list of missiles, placing it on the given img
(check-expect (render-lom empty (render-t (make-tank 100 0)))
              (render-t (make-tank 100 0)))
(check-expect (render-lom (list M1) (render-t T1))
              (place-image MISSILE
                           (missile-x M1)
                           (missile-y M1)
                           (render-t T1)))
(check-expect (render-lom (list M1 M2) (render-t T1))
              (place-image MISSILE
                           (missile-x M1)
                           (missile-y M1)
                           (place-image MISSILE
                                        (missile-x M2)
                                        (missile-y M2)
                                        (render-t T1))))
;(define (render-lom lom img) img) ;stub
(define (render-lom lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE
                      (missile-x  (first lom))
                      (missile-y  (first lom))
                      (render-lom (rest lom) img))]))

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
(check-expect (process-input BEGIN "up" ) BEGIN)
(check-expect (process-input BEGIN "left")
              (make-game (game-invaders BEGIN)
                         (fire "up" (tank-x (game-tank BEGIN)) (game-missiles BEGIN))
                         (move-tank (game-tank BEGIN) "left")))
(check-expect (process-input BEGIN "right")
              (make-game (game-invaders BEGIN)
                         (fire "right" (tank-x (game-tank BEGIN)) (game-missiles BEGIN))
                         (move-tank (game-tank BEGIN) "right")))
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