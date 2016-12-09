;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec03x) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec03x) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A Radius is a Number

; A CGWorld is (make-cgw Radius Number)
(define-struct cgw (radius score))
; interp. `radius` is the radius of the circle and `score`
; is the count of successful clicks

;Template for CGWorld:
#;
(define (process world ...)
  ... (cgw-radius world) ...
  ... (cgw-score world) ...)


; Constants
(define SIZE        400)         ; height and width of scene
(define SHRINK-RATE 2)           ; radius decrease at each time step
(define FONT-SIZE 20)            ; size of text
(define TEXT-COLOR "black")      ; the color for text
(define SAFE-COLOR "green")      ; large circle color
(define MARGINAL-COLOR "yellow") ; medium circle color
(define DANGER-COLOR "red")      ; small circle color
(define LOSE-MSG    "You lose!") ; message displayed upon loss

; Derived constants
(define CCX         (/ SIZE 2))      ; x coord
(define CCY         (/ SIZE 2))      ; y coord
(define R0          (* 3/8 SIZE))    ; initial radius
(define MARGINAL    (* 2/8 SIZE))    ; marginal radius
(define DANGER      (* 1/8 SIZE))    ; danger radius
(define GROW-RATE   (* 5 SHRINK-RATE)) ; radius increase at each click
(define TEXT-INSET  (/ FONT-SIZE 2)) ; how far from upper-left to put score
(define EMPTY       (empty-scene SIZE SIZE))



(define CG-WORLD0 (make-cgw R0 0))

; cg-stop? : CGWorld -> Boolean
; Determines when the game is over.
;
; Examples:
;  - When radius is 0, true.
;  - When radius is 1, false.
;
; Strategy: function composition
(define (cg-stop? world)
  (zero? (cgw-radius world)))

(check-expect (cg-stop? (make-cgw 0 10)) true)
(check-expect (cg-stop? (make-cgw 1 10)) false)
(check-expect (cg-stop? (make-cgw 5 10)) false)

; cg-draw : CGWorld -> Scene
; Renders the circle and score of the world.
;
; Examples:
;  - (make-world 50 600) renders a circle of radius 50 and a
;    score of 600.
;  - (make-world 0 600) renders the message "You lose!" and a
;    score of 600.
;
; Strategy: struct. decomp.
(define (cg-draw world)
  (overlay/xy
   (text (number->string (cgw-score world)) FONT-SIZE TEXT-COLOR)
   (- TEXT-INSET) (- TEXT-INSET)
   (place-image (cg-draw-radius (cgw-radius world))
                CCX CCY EMPTY)))

(check-expect (cg-draw (make-cgw (+ 1 MARGINAL) 17))
              (overlay/xy
               (text "17" FONT-SIZE TEXT-COLOR)
               (- TEXT-INSET) (- TEXT-INSET)
               (place-image (circle (+ 1 MARGINAL) "solid" SAFE-COLOR)
                            CCX CCY EMPTY)))
(check-expect (cg-draw (make-cgw 0 17))
              (overlay/xy
               (text "17" FONT-SIZE TEXT-COLOR)
               (- TEXT-INSET) (- TEXT-INSET)
               (place-image (text LOSE-MSG FONT-SIZE TEXT-COLOR)
                            CCX CCY EMPTY)))

; cg-draw-radius : Radius -> Image
; Renders the circle or loss message based on the radius.
;
; Examples:
;  - if radius = 0, the "You lose!" message
;  - if radius > MARGINAL, a big green circle
;  - if radius < DANGER, a smallish red circle
;
; Strategy: interval decomp.
(define (cg-draw-radius radius)
  (cond
    [(zero? radius) (text LOSE-MSG FONT-SIZE (rs->color radius))]
    [else           (circle radius "solid" (rs->color radius))]))

(check-expect (cg-draw-radius 0)
              (text LOSE-MSG FONT-SIZE "black"))
(check-expect (cg-draw-radius (+ 1 MARGINAL))
              (circle (+ 1 MARGINAL) "solid" "green"))
(check-expect (cg-draw-radius (+ 1 DANGER))
              (circle (+ 1 DANGER) "solid" "yellow"))
(check-expect (cg-draw-radius 1)
              (circle 1 "solid" "red"))


; rs->color : Radius -> Color
; Chooses a color to indicate the danger level of the given radius
;
; Examples:
;  - If r is MARGINAL + 1, green
;  - If r is MARGINAL, yellow
;  - If r is 2, red
;
; Strategy: interval decomp.
(define (rs->color r)
  (cond
    [(> r MARGINAL)  "green"]
    [(> r DANGER)    "yellow"]
    [(> r 0)         "red"]
    [else            "black"]))

(check-expect (rs->color (+ MARGINAL 1))  "green")
(check-expect (rs->color MARGINAL)        "yellow")
(check-expect (rs->color (+ DANGER 1))    "yellow")
(check-expect (rs->color DANGER)          "red")
(check-expect (rs->color 1)               "red")
(check-expect (rs->color 0)               "black")


; cg-tick : CGWorld -> CGWorld
; Shrinks the circle for each tick.
;
; Examples:
;  - (cg-tick (make-cgw 60 325)) => (make-cgw (- 60 SHRINK-RATE) 325)
;  - (cg-tick (make-cgw 0 325)) => (make-cgw 0 325)
;
; Strategy: struct. decomp.
(define (cg-tick world)
  (make-cgw (max 0 (- (cgw-radius world) SHRINK-RATE))
            (cgw-score world)))

(check-expect (cg-tick (make-cgw (+ 60 SHRINK-RATE) 325))
              (make-cgw 60 325))
(check-expect (cg-tick (make-cgw 1 325))
              (make-cgw 0 325))                       
(check-expect (cg-tick (make-cgw 0 325))
              (make-cgw 0 325))                       

; cg-mouse : CGWorld Number Number MouseEvt -> CGWorld
; Handles mouse events, responding to clicks in the circle.
;
; Examples:
;  - "move" event inside the circle => no change
;  - "button-up" event outside the circle => no change
;  - "button-up event inside the circle => radius increases by GROW-RATE
;    and score increases by 1
;
; Strategy: struct. decomp. (MouseEvt)
(define (cg-mouse world x y evt)
  (cond
    [(mouse=? "button-up" evt) (cg-mouse-click world x y)]
    [else world]))

(check-expect (cg-mouse (make-cgw 100 17) CCX CCY "move")
              (make-cgw 100 17))
(check-expect (cg-mouse (make-cgw 100 17) CCX (+ 150 CCY) "button-up")
              (make-cgw 100 17))
(check-expect (cg-mouse (make-cgw 100 17) CCX CCY "button-up")
              (make-cgw (+ 100 GROW-RATE) 18))
              

; cg-mouse-click : CGWorld Number Number -> CGWorld
; Updates the world based on a mouse click.
;
; Examples:
;  - inside circle => circle grows
(check-expect (cg-mouse-click (make-cgw 50 300) (+ 40 CCX) CCY)
              (make-cgw (+ GROW-RATE 50) 301))
;  - outside circle => no change
(check-expect (cg-mouse-click (make-cgw 50 300) (+ 60 CCX) CCY)
              (make-cgw 50 300))
;
; Strategy: decision tree
(define (cg-mouse-click world x y)
  (cond
    [(in-circ? x y (cgw-radius world) CCX CCY)
     (cg-mouse-click-inside world)]
    [else
     world]))


; cg-mouse-click-inside : CGWorld -> CGWorld
; Updates the world to respond to a click inside the circle.
;
; Examples:
(check-expect (cg-mouse-click-inside (make-cgw 46 9))
              (make-cgw (+ GROW-RATE 46) 10))
(check-expect (cg-mouse-click-inside (make-cgw 5 345))
              (make-cgw (+ GROW-RATE 5) 346))
;
; Strategy: struct. decomp.
(define (cg-mouse-click-inside world)
  (make-cgw (+ GROW-RATE (cgw-radius world))
            (+ 1 (cgw-score world))))


; in-circ? : Number Number Number Number Number -> Boolean
; Is the given point (x,y) within a circle of radius r
; centered at (cx,cy)?
;
; Examples:
;  - If center is (0, 0), click at (3, 4), and r = 3, false
;  - If center is (0, 0), click at (3, 4), and r = 6, true
;
; Strategy: domain knowledge (geometry)
(define (in-circ? x y r cx cy)
  (> r (dist (- cx x) (- cy y))))

(check-expect (in-circ? 0 0 5 0 0) true)
(check-expect (in-circ? 0 0 5 0 4) true)
(check-expect (in-circ? 0 0 5 0 5) false)
(check-expect (in-circ? 0 0 5 3 4) false)
(check-expect (in-circ? 0 1 5 3 4) true)


; dist: Number Number -> Number
; To compute the distance from (0, 0) to (x, y)
;
; Examples:
;  - distance to (3, 4) is 5
;  - distance to (5, 12) is 13
;
; Strategy: domain knowledge (geometry)
(define (dist x y)
  (sqrt (+ (sqr x) (sqr y))))

(check-expect (dist 0 0)  0)
(check-expect (dist 3 4)  5)
(check-expect (dist 9 12) 15)
(check-expect (dist -3 4) 5)



(big-bang
 CG-WORLD0
 (on-mouse  cg-mouse)
 (on-tick   cg-tick)
 (on-draw   cg-draw)
 (stop-when cg-stop?))
