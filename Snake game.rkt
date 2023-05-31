;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Snake game|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; snake-main : Gamestate String -> Gamestate
;; launches the game
;; to launch, type: (snake-main start "medium")
(define (snake-main gs tr)
  (big-bang gs
    [to-draw draw-world]
    [on-tick snake-moves
             (cond [(string=? tr "hard") .05]
                   [(string=? tr "medium") .1]
                   [(string=? tr "easy") .2])]
    [on-key control-snake]
    [stop-when game-lost? lose-screen]))




;;; Constants

(define WIDTH (sqr 24))
(define BACKGROUND (empty-scene WIDTH WIDTH))
(define DOT-DIAMETER (sqrt WIDTH))
(define DOT-RADIUS (/ (sqrt WIDTH) 2))
(define ONE-DOT (circle DOT-RADIUS "solid" "darkred"))
(define ONE-FOOD (circle DOT-RADIUS "solid" "green"))

;;; Data definitions

(define-struct dot [x y direction])
;; A Dot is a (make-struct Num Num String)
;; represents one piece of a Snake, has an x and y coordinate and is facing a certain direction
(define D1 (make-dot (- (/ WIDTH 2) (sqrt WIDTH)) (/ WIDTH 2) "right"))
(define D2 (make-dot (- (/ WIDTH 2) (* 2 (sqrt WIDTH))) (/ WIDTH 2) "right"))
(define D3 (make-dot (- (/ WIDTH 2) (* 3 (sqrt WIDTH))) (/ WIDTH 2) "right"))
(define D4 (make-dot (- (/ WIDTH 2) (* 3 (sqrt WIDTH))) (- (/ WIDTH 2) (sqrt WIDTH)) "down"))
(define D5 (make-dot (- (/ WIDTH 2) (* 4 (sqrt WIDTH))) (- (/ WIDTH 2) (* 1 (sqrt WIDTH))) "right"))
(define D6 (make-dot (- (/ WIDTH 2) (sqrt WIDTH)) (/ WIDTH 2) "up"))

;; A FoodPosn is a (make-posn Num Num)
;; represents the x and y coordinates of the food
(define F1 (make-posn (+ (dot-x D1) (* (sqrt WIDTH) 3)) (dot-y D1)))

(define-struct gamestate [snake food])
;; A GameState s a (make-struct [List-of Dot] FoodPosn)
;; represents the snake and the food and their positions
(define S1 (list D1))
(define S2 (list D1 D2 D3))
(define S3 (list D1 D2 D3))
(define S4 (list D1 D2 D3 D4))
(define S5 (list D1 D2 D3 D4 D5))
(define S6 (list D1 D2 D3 D4 D5 D6))

(define start (make-gamestate (list D1 D2 D3) F1))

;;; Functions

;; draw-world: GameState -> Image
;; draws the snake onto the background
(define (draw-world gs)
  (cond
    [(empty? (rest (gamestate-snake gs)))
     (place-image ONE-FOOD (posn-x (gamestate-food gs)) (posn-y (gamestate-food gs))
                  (place-image ONE-DOT (dot-x (first (gamestate-snake gs))) (dot-y (first (gamestate-snake gs)))
                               BACKGROUND))]
    [(cons? (rest (gamestate-snake gs)))
     (place-image ONE-FOOD (posn-x (gamestate-food gs)) (posn-y (gamestate-food gs))
                  (place-image ONE-DOT (dot-x (first (gamestate-snake gs))) (dot-y (first (gamestate-snake gs)))
                               (draw-world (make-gamestate (rest (gamestate-snake gs)) (gamestate-food gs)))))]))

;; snake-moves : Gamestate -> Gamestate
;; moves the snake on every tick and grows it if it eats a food
(define (snake-moves gs)
  (cond
    [(string=? (dot-direction (first (gamestate-snake gs))) "right")
     (if (collided? (gamestate-snake gs) (gamestate-food gs))
         (make-gamestate (eat-right (gamestate-snake gs)) (new-food gs))
         (make-gamestate (sm-right (gamestate-snake gs)) (gamestate-food gs)))]
    [(string=? (dot-direction (first (gamestate-snake gs))) "left")
     (if (collided? (gamestate-snake gs) (gamestate-food gs))
         (make-gamestate (eat-left (gamestate-snake gs)) (new-food gs))
         (make-gamestate (sm-left (gamestate-snake gs)) (gamestate-food gs)))]
    [(string=? (dot-direction (first (gamestate-snake gs))) "up")
     (if (collided? (gamestate-snake gs) (gamestate-food gs))
         (make-gamestate (eat-up (gamestate-snake gs)) (new-food gs))
         (make-gamestate (sm-up (gamestate-snake gs)) (gamestate-food gs)))]
    [(string=? (dot-direction (first (gamestate-snake gs))) "down")
     (if (collided? (gamestate-snake gs) (gamestate-food gs))
         (make-gamestate (eat-down (gamestate-snake gs)) (new-food gs))
         (make-gamestate (sm-down (gamestate-snake gs)) (gamestate-food gs)))]))

;; collided? : Snake FoodPosn -> Boolean
;; determines if the snake has eaten the food
(check-expect (collided? S3 (make-posn (- (/ WIDTH 2) (sqrt WIDTH)) (/ WIDTH 2))) #t)
(check-expect (collided? S3 (make-posn 0 0)) #f)
(define (collided? s f)
  (and (= (dot-x (first s)) (posn-x f))
       (= (dot-y (first s)) (posn-y f))))

;; new-food : Gamestate -> FoodPosn
;; makes a new food
(define (new-food gs)
  (make-posn (* (sqrt WIDTH) (+ 1 (random (- (sub1 (sqrt WIDTH)) 1))))
             (* (sqrt WIDTH) (+ 1 (random (- (sub1 (sqrt WIDTH)) 1))))))

;; sm-right : Snake -> Snake
(define (sm-right s)
  (cons
   (make-dot (+ (dot-x (first s)) DOT-DIAMETER)
             (dot-y (first s))
             (dot-direction (first s))) (reverse (rest (reverse s)))))
;; sm-left : Snake -> Snake
(define (sm-left s)
  (cons
   (make-dot (- (dot-x (first s)) DOT-DIAMETER)
             (dot-y (first s))
             (dot-direction (first s))) (reverse (rest (reverse s)))))
;; sm-up : Snake -> Snake
(define (sm-up s)
  (cons
   (make-dot (dot-x (first s))
             (- (dot-y (first s)) DOT-DIAMETER)
             (dot-direction (first s))) (reverse (rest (reverse s)))))
;; sm-down : Snake -> Snake
(define (sm-down s)
  (cons
   (make-dot (dot-x (first s))
             (+ (dot-y (first s)) DOT-DIAMETER)
             (dot-direction (first s))) (reverse (rest (reverse s)))))
;; eat-right : Snake -> Snake
(define (eat-right s)
  (cons
   (make-dot (+ (dot-x (first s)) DOT-DIAMETER)
             (dot-y (first s))
             (dot-direction (first s))) s))
;; eat-left : Snake -> Snake
(define (eat-left s)
  (cons
   (make-dot (- (dot-x (first s)) DOT-DIAMETER)
             (dot-y (first s))
             (dot-direction (first s))) s))
;; eat-up : Snake -> Snake
(define (eat-up s)
  (cons
   (make-dot (dot-x (first s))
             (- (dot-y (first s)) DOT-DIAMETER)
             (dot-direction (first s))) s))
;; eat-down : Snake -> Snake
(define (eat-down s)
  (cons
   (make-dot (dot-x (first s))
             (+ (dot-y (first s)) DOT-DIAMETER)
             (dot-direction (first s))) s))


;; control-snake : Gamestate KeyEvent -> Gamestate
(define (control-snake gs ke)
  (if (not (opposite-dir? (first (gamestate-snake gs)) ke))
      (make-gamestate (cons (make-dot (dot-x (first (gamestate-snake gs)))
                                      (dot-y (first (gamestate-snake gs))) ke)
                            (rest (gamestate-snake gs))) (gamestate-food gs))
      gs))

;; opposite-dir? : Dot -> Boolean
;; is the snake facing the opposite direction of the way you want it to go?
;; (the snake can't do a complete 180)
(check-expect (opposite-dir? D1 "left") #t)
(check-expect (opposite-dir? D2 "down") #f)
(check-expect (opposite-dir? D4 "up") #t)
(define (opposite-dir? d ke)
  (or (and (string=? (dot-direction d) "left") (key=? ke "right"))
      (and (string=? (dot-direction d) "right") (key=? ke "left"))
      (and (string=? (dot-direction d) "down") (key=? ke "up"))
      (and (string=? (dot-direction d) "up") (key=? ke "down"))))


;; game-lost? : Gamestate -> Boolean
(define (game-lost? gs)
  (local [(define EDGE (- WIDTH (/ (sqrt WIDTH) 2)))
          (define HALF-SQUARE (/ (sqrt WIDTH) 2))
          (define FRST-DOT (first (gamestate-snake gs)))]
    (or (ormap (λ(dot) (> (dot-x dot) EDGE)) (gamestate-snake gs))
        (ormap (λ(dot) (< (dot-x dot) HALF-SQUARE)) (gamestate-snake gs))
        (ormap (λ(dot) (> (dot-y dot) EDGE)) (gamestate-snake gs))
        (ormap (λ(dot) (< (dot-y dot) HALF-SQUARE)) (gamestate-snake gs))
        (ormap (λ(dot) (and (= (dot-x dot) (dot-x FRST-DOT))
                            (= (dot-y dot) (dot-y FRST-DOT)))) (rest (gamestate-snake gs))))))

;; lose-screen : Gamestate -> Image
(define (lose-screen gs)
  (cond
    [(game-lost? gs) (place-image (text (string-append "Game over! Score: " (number->string (length (gamestate-snake gs))))
                                        (sqrt WIDTH) "red") (/ WIDTH 2) (/ WIDTH 2) (draw-world gs))]))

     









