#lang racket

(require test-engine/racket-tests
         2htdp/image
         2htdp/universe)
(require (prefix-in gui: racket/gui))


; Press Run, then type (start) in the interactions window to play the game!

; Quick controls reference:
;  Space to start
;  WASD to move
;  Esc to pause and return to menu when paused
;  1, 2, 3, 4 to change difficulty


; ------------------------ Data definitions ---------------------------

;; A GameState is one of:
;; - 'main-menu
;; - 'playing
;; - 'paused
;; - 'lost
;; - 'won
;; Interpretation: used to remember current state of the game

(struct Rectangle [x y vx vy col wid hei] #:transparent)
; A Rectangle is a structured type: it contains
; - x, y: (Numbers) its position
; - vx, vy: (Numbers) its movement speed
; - col: its colour (a String)
; - wid, hei (Numbers): width and height of the rectangle.

(struct Player [cx cy keys] #:transparent)
; Player is a structured type which defines the position of the circle on the background
; It contains:
; - cx: a Number
; - cy: a Number
; - keys: a Keys, indicating which directional keys are currently pressed

(struct Keys [l r u d] #:transparent)
; Keys is a structured type. it contains:
; - l, r, u, d: to track which directional keys are currently pressed

(struct WorldState [timer hiscore difficulty gamestate player reclst] #:transparent)
; The WorldState is a structured type: it contains
; - timer: a Number, showing how many seconds have passed in a game
; - hiscore: an Integer, representing the highest obtained score in an instance of the game
; - difficulty: an Integer, affecting rectangle spawn rate, and movement speed of the rectangles and the player
; - gamestate: a GameState
; - player: a Player structured type
; - reclst: a list of currently shown rectangles


; ---------------------- Constant definitions -------------------------

(define WIDTH 800)
(define HEIGHT 800)
(define BG (empty-scene WIDTH HEIGHT))
(define PEN (pen "black" 5 "solid" "round" "round"))
(define COLOURS (list "red" "blue" "yellow" "green"))
(define NCOLOURS (length COLOURS))
(define PLAYER-WIDTH 24)
(define IMG-PLAYER (radial-star PLAYER-WIDTH 10 15 "outline" "darkslategray"))
(define PLAYZONE (Rectangle (/ WIDTH 2) (/ HEIGHT 2) 0 0 "white" (+ 200 WIDTH) (+ 200 HEIGHT)))
(define WIN-TIME 30)

(define (PAUSE-TEXT ws)
  (overlay (text/font "PAUSED" 32 "black" "Gill Sans" 'swiss 'normal 'bold #f)
           (text/font (string-append "\n\n\n\n\nScore: "
                                     (number->string (* (WorldState-difficulty ws)
                                                        (round (WorldState-timer ws)))))
                      18 "black" "Lucida Console" 'swiss 'normal 'bold #f)))

(define START-STATE (WorldState 0
                                0
                                1
                                'main-menu
                                (Player (/ WIDTH 2)
                                        (/ HEIGHT 2)
                                        (Keys #f #f #f #f))
                                empty))

(define MENU-IMAGE (scale (/ WIDTH (image-width (bitmap/file "MenuScreen.png"))) (bitmap/file "MenuScreen.png")))
(define LOST-IMAGE (bitmap/file "LostScreen.png"))
(define WON-IMAGE (bitmap/file "WonScreen.png"))
(define KILLSOUND "killsound.mp3")
(define TWINKLE "twinkle.mp3")


; The following are used for testing.
(define REC1 (Rectangle 400 300 2 1 "red" 40 20))
(define REC2 (Rectangle 200 100 -2 3 "yellow" 30 50))
(define REC3 (Rectangle 420 300 2 1 "blue" 40 20))
(define REC4 (Rectangle 1000 300 2 1 "black" 40 20))
(define REC5 (Rectangle 430 320 2 1 "green" 40 20))


; ------------------ Mondriaan -----------------------

; Number String Number Number -> Rectangle
; Given a number "dir" indicating one of the four screen edges, a colour,
; width and height, create a new rectangle at a random position along the
; given screen edge and with a random velocity towards the opposite side.
; dir: 0=left to right, 1=right to left, 2=top to bottom, 3=bottom to top
(define (random-rectangle-helper dir col wid hei)
  (cond [(= dir 0) (Rectangle (- wid)
                              (random HEIGHT)
                              (/ (round (* 100 (+ 1.25 (* 2 (random))))) 100)
                              0
                              col wid hei)]
        [(= dir 1) (Rectangle (+ WIDTH wid)  (random HEIGHT) (/ (round (* 100 (- (+ 1.25 (* 2 (random)))))) 100) 0 col wid hei)]
        [(= dir 2) (Rectangle (random WIDTH) (- hei)         0 (/ (round (* 100 (+ 1.25 (* 2 (random))))) 100)     col wid hei)]
        [(= dir 3) (Rectangle (random WIDTH) (+ HEIGHT hei)  0 (/ (round (* 100 (- (+ 1.25 (* 2 (random)))))) 100) col wid hei)]
        [else (error "There are only four directions!")]))

          
; -> Rectangle
; Generates a random rectangle
(define (random-rectangle)
  (random-rectangle-helper (random 4)
                           (list-ref COLOURS (random NCOLOURS))
                           (+ 50 (random 80))
                           (+ 50 (random 80))))


; Rectangle Image -> Image
; Draws a Rectangle on top of a given image
(define (draw-rectangle rec scene)
  (place-image (overlay (rectangle (Rectangle-wid rec) (Rectangle-hei rec) "outline" PEN)
                        (rectangle (Rectangle-wid rec) (Rectangle-hei rec) "solid" (Rectangle-col rec)))
               (Rectangle-x rec)
               (Rectangle-y rec)
               scene))

(check-expect (draw-rectangle REC1 BG)
              (place-image (overlay (rectangle (Rectangle-wid REC1) (Rectangle-hei REC1) "outline" PEN)
                                    (rectangle (Rectangle-wid REC1) (Rectangle-hei REC1) "solid" (Rectangle-col REC1)))
                           (Rectangle-x REC1)
                           (Rectangle-y REC1)
                           BG))


; (Listof Rectangle) -> Image
; Draws all the rectangles
(define (draw-all reclst)
  (cond [(empty? reclst) BG]
        [else (draw-rectangle (first reclst)
                              (draw-all (rest reclst)))]))

(check-expect (draw-all (list REC1 REC2))
              (draw-rectangle REC1 (draw-rectangle REC2 BG)))


; WorldState Rectangle -> Rectangle
; Moves a single rectangle according to its speed vector.
(define (move-one ws rec)
  (struct-copy Rectangle rec
               [x (round (+ (Rectangle-x rec) (* (WorldState-difficulty ws) (Rectangle-vx rec))))]
               [y (round (+ (Rectangle-y rec) (* (WorldState-difficulty ws) (Rectangle-vy rec))))]))

(check-expect (move-one START-STATE REC1)
              (Rectangle 402 301 2 1 "red" 40 20))

(check-expect (move-one START-STATE REC2)
              (Rectangle 198 103 -2 3 "yellow" 30 50))


; WorldState (Listof Rectangle) -> (Listof Rectangle)
; Moves all rectangles in the WorldState
(define (move-all ws reclst)
  (cond [(empty? reclst) empty]
        [else
           (cons (move-one ws (first reclst))
                 (move-all ws (rest reclst)))]))

(check-expect (move-all START-STATE (list REC1 REC2))
              (list (Rectangle 402 301 2 1 "red" 40 20)
                    (Rectangle 198 103 -2 3 "yellow" 30 50)))


; ------------------ Player controls -----------------------

; Keys String Boolean -> Keys
; Updates key presses
(define (update-keys keys key val)
  (cond [(string=? key "d") (struct-copy Keys keys [r val])]
        [(string=? key "a") (struct-copy Keys keys [l val])]
        [(string=? key "w") (struct-copy Keys keys [u val])]
        [(string=? key "s") (struct-copy Keys keys [d val])]
        [else (struct-copy Keys keys)]))


; ------------------- Helper Functions ------------------------

; WorldState GameState -> WorldState
; Updates gamestate
(define (set-state ws gs)
  (struct-copy WorldState ws
               [gamestate gs]))

(check-expect (set-state START-STATE 'playing) (struct-copy WorldState START-STATE
                                                             [gamestate 'playing]))


; WorldState GameState -> Boolean
; Returns a boolean based on which gamestate is active
(define (in-state ws gs)
  (equal? (WorldState-gamestate ws) gs))

(check-expect (in-state START-STATE 'main-menu) #t)


; WorldState -> Number
; Keeps track of high score
(define (high-score ws)
  (if (and (> (* (WorldState-difficulty ws) (round (WorldState-timer ws))) (WorldState-hiscore ws))
           (equal? (WorldState-difficulty ws) 4))
      (* (WorldState-difficulty ws) (round (WorldState-timer ws)))
      (WorldState-hiscore ws)))


; WorldState -> Number
; Adjusts spawn rate and movement speed of rectangles based on difficulty setting
(define (diff ws)
  (if (equal? (WorldState-difficulty ws) 4)
      (+ 2.5 (* (round (WorldState-timer ws)) 0.01))
      (WorldState-difficulty ws)))

(check-expect (diff START-STATE) 1)
   

; ------------------- Collision Detection ------------------------

; Rectangle Rectangle -> Boolean
; Returns #true when two rectangles overlap, #false otherwise
(define (overlaps? r1 r2)
  (and (< (abs (- (Rectangle-x r1) (Rectangle-x r2)))
          (+ (/ (Rectangle-wid r1) 2) (/ (Rectangle-wid r2) 2)))
       (< (abs (- (Rectangle-y r1) (Rectangle-y r2)))
          (+ (/ (Rectangle-hei r1) 2) (/ (Rectangle-hei r2) 2)))))

        
(check-expect (overlaps? REC1 REC2) #f)
(check-expect (overlaps? REC1 REC3) #t)
(check-expect (overlaps? REC3 REC5) #f)


; Rectangle -> Boolean
; Determines if the Rectangle is still in the play zone, or has flown off to eternity.
(define (visible? rec)
  (overlaps? rec PLAYZONE))

(check-expect (visible? REC1) #t)
(check-expect (visible? REC4) #f)


; (ListOf Rectangle) -> (ListOf Rectangle)
; Removes all rectangles that are not visible anymore
(define (remove-invisible reclst)
  (cond [(empty? reclst) empty]
        [else (if (visible? (first reclst))
                  (cons (first reclst)
                        (remove-invisible (rest reclst)))
                  (remove-invisible (rest reclst)))]))

(check-expect (remove-invisible (list REC4 REC2)) (list REC2))


; Player Rectangle -> Boolean
; Returns #true when the player overlaps with a single rectangle, #false otherwise
(define (collides? ply rec)
  (and (< (abs (- (Rectangle-x rec) (Player-cx ply)))
          (+ (/ (Rectangle-wid rec) 2) PLAYER-WIDTH -5))
       (< (abs (- (Rectangle-y rec) (Player-cy ply)))
          (+ (/ (Rectangle-hei rec) 2) PLAYER-WIDTH -5))))

(check-expect (collides? (Player 400 300 (Keys #f #f #f #f)) REC1) #t)


; Player (ListOf Rectangle) -> Boolean
; Returns #true when the player overlaps with any rectangle from a list, #false otherwise
(define (collision ply reclst)
  (cond [(empty? reclst) #f]
        [(collides? ply (first reclst)) #t]
        [else
           (collision ply (rest reclst))]))

(check-expect (collision (Player 400 300 (Keys #f #f #f #f)) (list REC1)) #t)


; ------------------- On Tick ------------------------

; WorldState -> WorldState
; Updates Player position as well as moving all rectangles and adding new ones at random intervals
; Checks that the player is still on the screen and not colliding with any rectangles
; Also updates timer and changes the GameState to 'won if WIN-TIME has been reached
(define (tick-playing ws)  
  (cond [(collision (WorldState-player ws) (WorldState-reclst ws)) (gui:play-sound KILLSOUND #t)
                                                                   (set-state ws 'lost)]

        [(not (collides? (WorldState-player ws) PLAYZONE)) (gui:play-sound KILLSOUND #t)
                                                           (set-state ws 'lost)]

        [(and (> (WorldState-timer ws) WIN-TIME)
              (not (equal? (WorldState-difficulty ws) 4))) (gui:play-sound TWINKLE #t)
                                                           (set-state (struct-copy WorldState START-STATE [difficulty (WorldState-difficulty ws)]) 'won)]

        [else (struct-copy WorldState ws

                       [timer (+ 1/28 (WorldState-timer ws))]

                       [player (struct-copy Player (WorldState-player ws)

                                            [cx (+ (Player-cx (WorldState-player ws))
                                                   (if (Keys-l (Player-keys (WorldState-player ws))) (+ (* -1 (diff ws)) -5) 0)
                                                   (if (Keys-r (Player-keys (WorldState-player ws))) (+ (diff ws) +5) 0))]

                                            [cy (+ (Player-cy (WorldState-player ws))
                                                   (if (Keys-u (Player-keys (WorldState-player ws))) (+ (* -1 (diff ws)) -5) 0)
                                                   (if (Keys-d (Player-keys (WorldState-player ws))) (+ (diff ws) +5) 0))])]

                       [reclst (cond [(< (random) (* (diff ws) 0.05)) (cons (random-rectangle)
                                                                                             (move-all ws (remove-invisible (WorldState-reclst ws))))]
                                     [else (move-all ws (WorldState-reclst ws))])])]))


; WorldState -> WorldState
; Handles tick update in all states
(define (on-tick-handler ws)
  (cond [(in-state ws 'playing) (tick-playing ws)]
        [else ws]))


; ------------------- To Draw ------------------------

; WorldState -> Image
; Draws everything on the background for 'playing gamestate
; Includes text to track timer and difficulty as well as drawing the player and all the rectangles
(define (draw-playing ws)
  (place-image (text/font (string-append "Elapsed time: "
                                         (number->string (round (WorldState-timer ws)))
                                         "\n   Level: "
                                         (number->string (WorldState-difficulty ws)))
                                   12 "black" "Lucida Console" 'swiss 'normal 'bold #f)
                        720
                        25
                        (place-image IMG-PLAYER
                                     (Player-cx (WorldState-player ws))
                                     (Player-cy (WorldState-player ws))
                                     (draw-all (WorldState-reclst ws)))))


; WorldState -> Image
; Draws the 'paused state (text and reduced opacity of 'playing state)
(define (draw-paused ws)
  (overlay (PAUSE-TEXT ws)
           (square WIDTH 140 "white")
           (draw-playing ws)))


; WorldState -> Image
; Draws the 'main-menu state
; Includes text to show current level and difficulty
(define (draw-menu ws)
  (place-image (text/font (number->string (WorldState-difficulty ws))
                          24 "black" "Gill Sans" 'swiss 'normal 'bold #f)
               (+ 8 (/ WIDTH 2))
               (- HEIGHT 285)
               (place-image (text/font (number->string (WorldState-hiscore ws))
                                       24 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                            (+ 5 (/ WIDTH 2))
                            (- HEIGHT 160)
                            MENU-IMAGE)))


; WorldState -> Image
; Draws the 'lost state
(define (draw-lost ws)
  (overlay (text/font (string-append "\n\n\n\n\n\n\n\n\n\n\n\n\n\nScore: "
                                     (number->string (* (WorldState-difficulty ws)
                                                        (round (WorldState-timer ws)))))
                 20 "white" "Lucida Console" 'swiss 'normal 'bold #f)
           LOST-IMAGE))


; WorldState -> Image
; Draws the 'won state
(define (draw-won ws)
  (overlay (text/font (string-append "\n\n\n\n\n\n\n\nYou survived " (number->string WIN-TIME) " seconds and completed level "
                                     (number->string (WorldState-difficulty ws)))
                 20 "black" "Lucida Console" 'swiss 'normal 'bold #f)
           (if (equal? (WorldState-difficulty ws) 3)
               (text/font "\n\n\n\n\n\n\n\n\n\n\n\n\n     The next level is endless,\n just try to beat your high score!"
                 20 "black" "Lucida Console" 'swiss 'normal 'bold #f)
               empty-image)
           WON-IMAGE))


; WorldState -> Image
; Draws everything on the screen for given WorldState
(define (to-draw-handler ws)
  (cond [(in-state ws 'playing) (draw-playing ws)]
        [(in-state ws 'paused) (draw-paused ws)]
        [(in-state ws 'main-menu) (draw-menu ws)]
        [(in-state ws 'lost) (draw-lost ws)]
        [(in-state ws 'won) (draw-won ws)]))
        


; ------------------- On Key ------------------------

; WorldState String -> WorldState
; Handles key presses during 'playing GS
; Updates Keys to #true when pressed down
(define (key-playing ws key)
  (if (equal? key "escape") (set-state ws 'paused)
   (struct-copy WorldState ws
                [player (struct-copy Player (WorldState-player ws)
                                     [keys (update-keys (Player-keys (WorldState-player ws)) key #t)])])))


; WorldState String -> WorldState
; Handles key presses during 'paused GS
; Allows return to 'main-menu and 'playing gamestates
(define (key-paused ws key)
  (cond [(equal? key "escape") (set-state ws 'main-menu)]
        [(equal? key " ") (set-state ws 'playing)]
        [else ws]))


; WorldState String -> WorldState
; Handles key presses during 'main-menu GS
; Updates difficulty and gamestate
(define (key-menu ws key)
  (cond [(equal? key " ") ; (gui:play-sound MUSIC #t)
                          (set-state (struct-copy WorldState START-STATE
                                                  [difficulty (WorldState-difficulty ws)]
                                                  [hiscore (high-score ws)]) 'playing)]
        [(equal? key "1") (struct-copy WorldState ws [difficulty 1])]
        [(equal? key "2") (struct-copy WorldState ws [difficulty 2])]
        [(equal? key "3") (struct-copy WorldState ws [difficulty 3])]
        [(equal? key "4") (struct-copy WorldState ws [difficulty 4])]
        [else ws]))


; WorldState String -> WorldState
; Handles key presses during 'lost GS
; Restarts 'playing state or returns to 'main-menu
(define (key-lost ws key)
  (cond [(equal? key "escape") (set-state (struct-copy WorldState ws
                                                       [hiscore (high-score ws)]) 'main-menu)]
        [(equal? key " ") (set-state (struct-copy WorldState START-STATE
                                                  [difficulty (WorldState-difficulty ws)]
                                                  [hiscore (high-score ws)]) 'playing)]
        [else ws]))


; WorldState String -> WorldState
; Handles key presses during 'won GS
; Returns to 'main-menu or starts next level
(define (key-won ws key)
  (cond [(equal? key "escape") (set-state ws 'main-menu)]
        [(equal? key " ") (set-state (struct-copy WorldState START-STATE
                                                  [difficulty (+ 1 (WorldState-difficulty ws))]
                                                  [hiscore (high-score ws)]) 'playing)]
        [else ws]))


; WorldState String -> WorldState
; Handles key presses at all times
(define (on-key-handler ws key)
  (cond [(in-state ws 'playing) (key-playing ws key)]
        [(in-state ws 'paused) (key-paused ws key)]
        [(in-state ws 'main-menu) (key-menu ws key)]
        [(in-state ws 'lost) (key-lost ws key)]
        [(in-state ws 'won) (key-won ws key)]
        [else ws]))


; WorldState String -> WorldState
; Handles key release during 'playing GS
; Updates Keys to #false when released
(define (on-release-handler ws key)
  (if (in-state ws 'playing)
      (struct-copy WorldState ws
              [player (struct-copy Player (WorldState-player ws)
                                   [keys (update-keys (Player-keys (WorldState-player ws)) key #f)])]) 
      ws))


; ------------------- Big Bang ------------------------

; -> WorldState
; Starts the game
(define (start)
  (big-bang START-STATE
    [to-draw    to-draw-handler]
    [on-key     on-key-handler]
    [on-release on-release-handler]
    [on-tick    on-tick-handler]))



(test)