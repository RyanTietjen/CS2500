;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8-prob2done) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IMPORTANT:
; 1. The functions that you design for this problem *must* use the ISL list
;    abstraction(s); you MAY NOT use recursion: doing so will lead you to get
;    no code credit for the function :(
;
; 2. The planning part of this problem is in the SAME file used for ALL parts
;    of Homework 8.

; Your task here will be to design the function elim-contains-char, which takes
; a list of strings and produces a list of the same strings, in the same order,
; but excluding those strings that contain a supplied character (represented as
; a 1String). For clarity, here is the intended signature:

; elim-contains-char : 1String [List-of String] -> [List-of String]


; Note: For purposes of this problem, you should NOT use string-contains? (or
;       similar). Instead, use the explode function to treat the supplied
;       string as a list of characters (each represented as a 1String).


; TODO 1/2: Plan your solution, using the planning interface described on the
;           canvas page for this assignment. ALL PLANNING FOR THIS HW WILL BE
;           DONE IN THE SAME PLACE, AND SUBMITTED TOGETHER.

; TODO 2/2: Design the function elim-contains-char using the ISL list
;           abstractions. YOUR CODE SHOULD NOT USE ANY RECURSION.

(define LOS-1 (list "hi" "bruh" "21savage"))
(define LOS-2 (list "i" "ball" "like" "tatum"))

(check-expect (elim-contains-char "2" LOS-1) (list "hi" "bruh"))
(check-expect (elim-contains-char "l" LOS-2) (list "i" "tatum"))
(check-expect (elim-contains-char "R" LOS-1) (list "hi" "21savage"))
(check-expect (elim-contains-char "O" (list '())) '())

; elim-contains-char : n [List-of String] -> [List-of String]
; removes all strings that contain a given character from a given list of strings

(define (elim-contains-char n los)
  (local [; inlist? : String -> String
          ; returns #false if a given list of
          ; 1strings contains a given 1string
          (define (inlist? los-explode)
            (andmap (λ (p) (not (string=? n p))) los-explode))]
    (map implode (filter inlist? (map explode los)))))





