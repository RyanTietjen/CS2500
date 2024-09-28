;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw1-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Understanding code that doesn't have effective naming and documentation
; can be really challenging! (That's part of the reason that this class, and
; most organizations with software-development teams, employ style guides.)

; As an example, consider the confusing functions defined below...

; Hints:
;  - For built-in functions that aren’t familiar to you,
;    be sure to look them up in the DrRacket documentation!
;  - Once you have a theory about what a function is doing, try
;    running it in the interactions window, then try changing
;    parameters to confirm how it all works!
;  - You might start with smaller, simpler functions, and
;    then that can help understand bigger ones (that use them!)


; nonplus1: String Nat -> String
; Capitalizes a string and adds a given number of exclamation marks to the end
(define (nonplus1 a b)
  (string-append
   (string-upcase a)
   (replicate b "!")))

(define NONPLUS1-EXPECTED (nonplus1 "hello" 5))
(define NONPLUS1-ACTUAL "HELLO!!!!!")
(check-expect NONPLUS1-EXPECTED NONPLUS1-ACTUAL)


; nonplus2: Real String Real -> Int
; Given two numbers and a string, this function finds the maximum between a number and
; the length of a string, then finds the minimum between that and another number
(define (nonplus2 a b c)
  (min
   (max
    (string-length b)
    a)
   c))

(define NONPLUS2-EXPECTED-A (nonplus2 1 "kr" 3))
(define NONPLUS2-ACTUAL-A 2)
(check-expect NONPLUS2-EXPECTED-A NONPLUS2-ACTUAL-A)

(define NONPLUS2-EXPECTED-B (nonplus2 -1 "kr" -3))
(define NONPLUS2-ACTUAL-B -3)
(check-expect NONPLUS2-EXPECTED-B NONPLUS2-ACTUAL-B)

; String Nat Color -> image
; PURPOSE HERE
(define (nonplus3 a b c)
  (above
   (text (nonplus1 a b) 30 c)
   (text (replicate (nonplus2 b a 5) "⭐") 20 c)))

(define NONPLUS3-EXPECTED-A (nonplus3 "kr" 7 "blue"))
(define NONPLUS3-ACTUAL-A
  (above
   (text "KR!!!!!!!" 30 "blue")
   (text "⭐⭐⭐⭐⭐" 20 "blue")))

(check-expect NONPLUS3-EXPECTED-A NONPLUS3-ACTUAL-A)

(define NONPLUS3-EXPECTED-B (nonplus3 "kr" 3 "red"))
(define NONPLUS3-ACTUAL-B
  (above
   (text "KR!!!" 30 "red")
   (text "⭐⭐⭐" 20 "red")))
(check-expect NONPLUS3-EXPECTED-B NONPLUS3-ACTUAL-B)

  



; TODO 1/2: Replace each "SIGNATURE HERE" and "PURPOSE HERE" with signature
;           and purpose statements for that function. Remember, a purpose
;           statement should say *what* a function does, not just re-state
;           the signature; the signature precisely *how* to use the function,
;           that is, what type(s) of data need to be supplied (in what order)
;           and what type of data will be returned. In some sense, the purpose
;           helps someone figure out if they will find a function useful, and
;           then the signature helps them use it.

; TODO 2/2: Confirm the above TODO by defining at least one pair of constants
;           per function, according to the following rules...
;           - In each pair, end the name of one -ACTUAL and the other -EXPECTED;
;             for example, NONPLUS1-ACTUAL and NONPLUS1-EXPECTED
;           - The "ACTUAL" value should call the function with a set of arguments
;             you select, which must adhere to your signature; the "EXPECTED"
;             should be the value (like "hi!") or expression (like (text "hi!" 5 "black"))
;             you expect ACTUAL to be when executed;
;             for example, the value for NONPLUS1-ACTUAL could be (nonplus1 3 4)
;             and the value for NONPLUS1-EXPECTED could be 7, if you thought the
;             function added two numeric arguments
;           - If you would like to demonstrate multiple actual/expected pairs, you are
;             welcome, in which case just be reasonable in how you name them (e.g.,
;             NONPLUS1-ACTUAL-A, NONPLUS1-EXPECTED-A)
;           - Now run your program and make sure each ACTUAL/EXPECTED pair is equal
;             (you can do this in the intermediate window, or using a check-expect)
;
;           Note: as we proceed in the course, you'll be doing this a lot in the form
;           of tests, since they are helpful to document your code, as well as make
;           sure it works the way you think it does!




