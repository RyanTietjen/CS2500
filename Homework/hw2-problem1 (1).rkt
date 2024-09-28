;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw2-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/1: Design the function valid-5mc? that determines if a supplied
;           1-character string (a 1String) is a valid response to a 5-option
;           multiple-choice question, either upper- or lower-case. So "A",
;           "c" and "E" are all valid (and so the function should return #true;
;           whereas "f", "7", "?", and "Z" are all invalid (and so the function
;           should return #false).
;
;           Be sure you follow all steps of the design recipe and include
;           check-expects that cover both result values, as well as making sure
;           upper- and lower-case examples work properly.
;
;           Importantly, you should NOT use code that follows the pattern...
;
;              (if expression #true #false)
;
;           since this could be replaced with just expression


; A MultipleChoice is one of:
; "A"
; "B"
; "C"
; "D"
; "E"
; Interpretation: A valid response to a 5-option multiple choice question

; Examples:
(define MC-A "A")
(define MC-B "B")
(define MC-C "C")
(define MC-D "D")
(define MC-E "E")

; Template:
#;(define (multiple-choice-temp str)
    (cond [(string=? str MC-A) ...]
          [(string=? str MC-B) ...]
          [(string=? str MC-C) ...]
          [(string=? str MC-D) ...]
          [(string=? str MC-E) ...]))


; valid-5mc? : 1String -> Boolean
; Determines if a character is a valid response to a 5-option multiple-choice question
(check-expect (valid-5mc? "A") #t)
(check-expect (valid-5mc? "b") #t)
(check-expect (valid-5mc? "c") #t)
(check-expect (valid-5mc? "D") #t)
(check-expect (valid-5mc? "E") #t)
(check-expect (valid-5mc? "f") #f)
(check-expect (valid-5mc? "7") #f)
(check-expect (valid-5mc? "?") #f)
(check-expect (valid-5mc? "z") #f)

(define (valid-5mc? str)
  (cond [(string=? (string-upcase str) MC-A) #t]
        [(string=? (string-upcase str) MC-B) #t]
        [(string=? (string-upcase str) MC-C) #t]
        [(string=? (string-upcase str) MC-D) #t]
        [(string=? (string-upcase str) MC-E) #t]
        [else #f]))