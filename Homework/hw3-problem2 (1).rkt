;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Back to Wordle (https://www.nytimes.com/games/wordle/)!

; Recall that in HW2 you designed the type LetterStatus, an enumeration
; that categorized letters as correct, misplaced, or wrong.

; TODO 1/1: Design LetterStatusPair - data that represents pairing a letter
;           (1String) with its status (LetterStatus). Example values might
;           include that the the letter "A" is correct, "B" is wrong, or
;           "C" is misplaced.
;
;           Notes:
;           - You are welcome to use your own (correct) design of
;             LetterStatus, or ours (once released); either way, include
;             it below as a part of your solution to this problem.
;           - Follow all steps of the design recipe for data and remember
;             that in templates, if the type of a field is a data definition,
;             you need to call its associated template!


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
(define LSP-1 (make-lsp "B" LS-INCORRECT))
(define LSP-1 (make-lsp "C" LS-MISPLACED))

(define (lsp-temp lsp)
  (... (lsp-str lsp) ...
       (letter-status-temp (lsp-ls lsp)) ...))

; make-lsp: 1String LetterStatus -> LetterStatusPair
; lsp?: Any -> Boolean
; lsp-temp: LetterStatusPair -> ?
; lsp-str: LetterStatusPair -> String
; lsp-ls: LetterStatusPair -> LetterStatus
