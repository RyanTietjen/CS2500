;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's return to Wordle (https://www.nytimes.com/games/wordle/)!

; Eventually we'll want to visualize a prior guess, which means categorizing
; each letter as either correct (i.e., used letter in the correct spot),
; misplaced (i.e., used letter, but in the wrong spot), or wrong (i.e.,
; the letter is not in the word in any spot).

; TODO 1/2: Design the data type LetterStatus (LS), which represents the three
;           categories of letters described above. Make sure to follow all steps
;           of the design recipe for data!


; TODO 2/2: Design the function ls->color, which accepts a LetterStatus and
;           produces an associated Color (an existing type - look up the
;           image-color? function for details and examples). In the NYT game,
;           correct letters are green, misplaced letters are yellow, and wrong
;           letters are gray - feel free to choose the shades of these colors
;           that bring you happiness :) And make sure to follow all steps of
;           the design recipe for functions!


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


; ls->color: LetterStatus -> Color
; produces an associated color given the status of a letter
(check-expect (ls->color LS-CORRECT) "green")
(check-expect (ls->color LS-MISPLACED) "yellow")
(check-expect (ls->color LS-INCORRECT) "gray")

(define (ls->color str)
  (cond [(string=? str LS-CORRECT) "green"]
        [(string=? str LS-MISPLACED) "yellow"]
        [(string=? str LS-INCORRECT) "gray"]))