;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Back to Wordle (https://www.nytimes.com/games/wordle/)!

; We are going to put together some work you did in prior assignments...
; - In HW1, you made code for a boxed-letter function, which could create a
;   letter in a box, given lots of specific details (about color, size, etc).
; - In HW2, you designed an ls->color function, which would return the color
;   associated with the status of a letter.
; - In HW3, you designed the LetterStatusPair data type, which could represent
;   the pairing of a letter with its status.

; Moving towards the full game, it's going to be handy to be able to more
; simply draw various boxed letters (e.g., those that are part of the current
; guess, as well as those that were previously guessed). So let's build & use
; some abstractions :)

; TODO 1/2: Re-design boxed-letter to work a bit differently: instead of being
;           supplied details of how to draw the box (e.g., background/border
;           colors), it takes a function to make one (given a size), as well as
;           a letter, and then places the letter onto the background produced
;           by the function (with a font size proportional to the box size).
;
;           For clarity, you have been provided a function to produce a "blank"
;           background (useful while typing a guess) - when combined with your
;           new boxed-letter function, the supplied test should pass. You should
;           supplement this test with others in your design.


(define BG-COLOR "white")
(define BORDER-COLOR "dimgray")
(define GUESS-COLOR "black")

; blank : NonNegReal -> Image
; produces a blank box in the appropriate size

(check-expect (blank 5)
              (overlay (square 5 "outline" BORDER-COLOR)
                       (square 5 "solid" GUESS-COLOR)))

(check-expect (blank 7)
              (overlay (square 7 "outline" BORDER-COLOR)
                       (square 7 "solid" GUESS-COLOR)))

(define (blank size)
  (overlay (square size "outline" BORDER-COLOR)
           (square size "solid" GUESS-COLOR)))


;; boxed-letter: 1String Number [NonNegReal -> Image] -> Image
;; interpretation: creates a letter in a box
(check-expect
 (boxed-letter "B" 64 blank)
 (overlay
  (text "B" 32 BG-COLOR)
  (square 64 "outline" BORDER-COLOR)
  (square 64 "solid" GUESS-COLOR)))

(check-expect
 (boxed-letter "C" 24 blank)
 (overlay
  (text "C" 12 BG-COLOR)
  (square 24 "outline" BORDER-COLOR)
  (square 24 "solid" GUESS-COLOR)))

(check-expect
 (boxed-letter "A" 128 blank)
 (overlay
  (text "A" 64 BG-COLOR)
  (square 128 "outline" BORDER-COLOR)
  (square 128 "solid" GUESS-COLOR)))

(define (boxed-letter str num op)
  (overlay
   (text str (/ num 2) BG-COLOR)
   (op num)))




; TODO 2/2: Now, use your boxed-letter abstraction to design two useful
;           functions: guess-letter->image and lsp->image. The former
;           produces a visualization of a letter on a blank background,
;           while the latter produces a visualization of a LetterStatusPair
;           (LSP) on a background associated with he letter's status.
;           You have been supplied some tests for clarity (which you can use
;           in your design, but should supplement).
;
;           You are welcome to use your own (correct) design of LetterStatus,
;           LetterStatusPair, or ours; either way, include it below as a part
;           of your solution to this problem. (Note that you might have to
;           adjust your constants and structure name to accommodate the
;           supplied tests.)

; A LetterStatus is one of:
; "correct"
; "misplaced"
; "incorrect"
; Interpretation: A correct, misplaced, or incorrect letter guess in the Wordle game

; Examples:
(define LS-CORRECT "correct")
(define LS-MISPLACED "misplaced")
(define LS-INCORRECT "incorrect")

; Template:
#;(define (letter-status-temp str)
    (cond [(string=? str LS-CORRECT) ...]
          [(string=? str LS-MISPLACED) ...]
          [(string=? str LS-INCORRECT) ...]))



(define-struct lsp [str ls])

; A LetterStatusPair is a (make-lsp 1String LetterStatus)
; Interpretation: a letter paired with its status (LetterStatus)

(define LSP-1 (make-lsp "A" LS-CORRECT))
(define LSP-2 (make-lsp "B" LS-INCORRECT))
(define LSP-3 (make-lsp "C" LS-MISPLACED))

(define (lsp-temp lsp)
  (... (lsp-str lsp) ...
       (letter-status-temp (lsp-ls lsp)) ...))

; make-lsp: 1String LetterStatus -> LetterStatusPair
; lsp?: Any -> Boolean
; lsp-temp: LetterStatusPair -> ?
; lsp-str: LetterStatusPair -> String
; lsp-ls: LetterStatusPair -> LetterStatus


(define LT-SIZE 64)

;; guess-letter->image: 1String -> Image
;; produces a visualization of a letter on a blank background
(check-expect
 (guess-letter->image "A")
 (overlay
  (text "A" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(check-expect
 (guess-letter->image "B")
 (overlay
  (text "B" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(check-expect
 (guess-letter->image "C")
 (overlay
  (text "C" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "outline" BORDER-COLOR)
  (square LT-SIZE "solid" GUESS-COLOR)))

(define (guess-letter->image letter)
  (boxed-letter letter LT-SIZE blank))


;; lsp->image: LSP -> Image
;; interpretation: produces a visualization of a LetterStatusPair (LSP)
;; on a background associated with the letter's status
(check-expect
 (lsp->image (make-lsp "A" LS-CORRECT))
 (overlay
  (text "A" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "darkgreen")))

(check-expect
 (lsp->image (make-lsp "B" LS-INCORRECT))
 (overlay
  (text "B" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "dimgray")))

(check-expect
 (lsp->image (make-lsp "C" LS-MISPLACED))
 (overlay
  (text "C" (/ LT-SIZE 2) BG-COLOR)
  (square LT-SIZE "solid" "goldenrod")))

(define (lsp->image lsp)
  (boxed-letter (lsp-str lsp) LT-SIZE (box-color (lsp-ls lsp))))


;; box-color: LetterStatus -> [NonNegReal -> Image]
;; decides which box-drawing function should be used based on a LetterStatus
(define (box-color ls)
  (cond [(string=? ls LS-CORRECT) draw-correct]
        [(string=? ls LS-INCORRECT) draw-incorrect]
        [(string=? ls LS-MISPLACED) draw-misplaced]))

;; draw-correct: NonNegReal -> Image
;; interpretation; draws the background of a box when the LetterStatus is correct
(check-expect (draw-correct LT-SIZE) (square LT-SIZE "solid" "darkgreen"))
(check-expect (draw-correct 48) (square 48 "solid" "darkgreen"))
(define (draw-correct size)
  (draw-color-box size "darkgreen"))

;; draw-incorrect: NonNegReal -> Image
;; interpretation; draws the background of a box when the LetterStatus is incorrect
(check-expect (draw-incorrect LT-SIZE) (square LT-SIZE "solid" "dimgray"))
(check-expect (draw-incorrect 48) (square 48 "solid" "dimgray"))
(define (draw-incorrect size)
  (draw-color-box size "dimgray"))


;; draw-misplaced: NonNegReal -> Image
;; interpretation; draws the background of a box when the LetterStatus is misplaced
(check-expect (draw-misplaced LT-SIZE) (square LT-SIZE "solid" "goldenrod"))
(check-expect (draw-misplaced 48) (square 48 "solid" "goldenrod"))
(define (draw-misplaced size)
  (draw-color-box size "goldenrod"))


;; draw-color-box: NonNegReal Color -> Image
;; interpretation: draws the background of a box with a certain color
(check-expect (draw-color-box LT-SIZE "green") (square LT-SIZE "solid" "green"))
(check-expect (draw-color-box 44 "orange") (square 44 "solid" "orange"))
(define (draw-color-box size bc)
  (square size "solid" bc))
