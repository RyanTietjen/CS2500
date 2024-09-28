;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; It's super useful to be able to answer the question: does a value appear
; in a list? But of course that question can be phrased multiple ways...

; TODO 1/3: Design two functions: string-in-list? and string-in-list-ci?.
;           The first returns #true if a supplied string appears
;           *exactly* in a list of strings, whereas the second returns
;           #true if a supplied string occurs in a list of strings if
;           we ignore lower/upper-case. You have been supplied some tests
;           for clarity (which you can use in your design, but should
;           supplement). Make sure your code follows the list template!

;; string-in-list/old? : 1String [List-of String] -> Boolean
;; interpretation: returns true if a a supplied string occurs exactly in a list of strings
(check-expect (string-in-list?/old "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list?/old "c" (list "a" "b" "c")) #t)
(check-expect (string-in-list?/old "A" (list "a" "b" "c")) #f)
(check-expect (string-in-list?/old "a" (list)) #f)
(define (string-in-list?/old str alos)
  (cond [(empty? alos) #false]
        [(cons? alos)
         (if (string-contains? str (first alos))
             #true
             (string-in-list?/old str (rest alos)))]))

;; string-in-list-ci/old? : 1String [List-of String] -> Boolean
;; interpretation: returns true if a a supplied string occurs in a list of strings
;; regardless of capitalization 
(check-expect (string-in-list-ci?/old "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci?/old "A" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci?/old "c" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci?/old "a" (list)) #f)
(define (string-in-list-ci?/old str alos)
  (cond [(empty? alos) #false]
        [(cons? alos)
         (if (string-contains-ci? str (first alos))
             #true
             (string-in-list-ci?/old str (rest alos)))]))




; TODO 2/3: Those two functions probably feel rather similar - so now
;           design the abstraction value-in-list? based on these two
;           functions.

;           Notes:
;           - Think through your signature to make sure it is as general
;             as possible, while still not making promises your abstraction
;             cannot keep!
;           - Don't forget to improve your implementations for the last
;             step! (Importantly: keep the old code by renaming the
;             functions string-in-list?/old and string-in-list-ci?/old;
;             you do not need to change/reproduce any parts of the function
;             design recipe for these old function implementations.)

;; value-in-list? : (X Y) [X List-of Y -> Boolean] X [List-of Y] -> Boolean
;; determines if at least one item in a list passes a condition dependent on another value
(check-expect (value-in-list string-contains? "a" (list "a" "b" "c")) #t)
(check-expect (value-in-list string-contains? "c" (list "a" "b" "c")) #t)
(check-expect (value-in-list string-contains? "A" (list "a" "b" "c")) #f)
(check-expect (value-in-list string-contains? "a" (list)) #f)
(check-expect (value-in-list string-contains-ci? "a" (list "a" "b" "c")) #t)
(check-expect (value-in-list string-contains-ci? "A" (list "a" "b" "c")) #t)
(check-expect (value-in-list string-contains-ci? "c" (list "a" "b" "c")) #t)
(check-expect (value-in-list string-contains-ci? "a" (list)) #f)

(define (value-in-list op value alox)
  (cond [(empty? alox) #false]
        [(cons? alox)
         (if (op value (first alox))
             #true
             (value-in-list op value (rest alox)))]))

;; string-in-list? : 1String [List-of String] -> Boolean
;; interpretation: returns true if a a supplied string occurs exactly in a list of strings
(check-expect (string-in-list? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list? "c" (list "a" "b" "c")) #t)
(check-expect (string-in-list? "A" (list "a" "b" "c")) #f)
(check-expect (string-in-list? "a" (list)) #f)
(define (string-in-list? str alos)
  (value-in-list string-contains? str alos))

;; string-in-list-ci? : 1String [List-of String] -> Boolean
;; interpretation: returns true if a a supplied string occurs in a list of strings
;; regardless of capitalization 
(check-expect (string-in-list-ci? "a" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci? "A" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci? "c" (list "a" "b" "c")) #t)
(check-expect (string-in-list-ci? "a" (list)) #f)
(define (string-in-list-ci? str alos)
  (value-in-list string-contains-ci? str alos))


; TODO 3/3: Now put your fancy new abstraction to good use! Design the function
;           anything-bigger? that determines if any of a list of numbers is
;           bigger than a supplied number. You have been supplied some tests
;           for clarity (which you can use in your design, but should supplement).

;; anything-bigger? : Number [List-of Number] -> boolean
;; determines if any number in a list of numbers is bigger than a supplied number 
(check-expect (anything-bigger? 5 (list 10 -1 3)) #t)
(check-expect (anything-bigger? 100 (list 10 -1 3)) #f)

(define (anything-bigger? num alon)
  (value-in-list < num alon))



