;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definitions & interpretations...


(define-struct address [num st city us-state zip])

; An Address is a (make-address Nat String String String Nat)
; - num is the number of the building on the street
; - st is the name of the street
; - city is the city the building is in
; - us-state is the state the city is in
; - zip is the zipcode of the building
; Interpretation: a US address

(define ADDRESS-1 (make-address 525 "Forsyth" "Boston" "MA" 12345))
(define ADDRESS-2 (make-address 30 "Smith" "Freehold" "NJ" 07728))

#;(define (address-temp ad)
    (... (address-num ad) ... (address-st ad) ...
         (address-city ad) ... (address-us-state ad) ...
         (address-zip ad) ...))

(define-struct student [first last nuid local perm])

; An NUStudent is a (make-student String String PositiveNumber Address Address)
; - first is the student's first name
; - last is the student's last name
; - nuid is the student's NUID
; - local is the student's local address
; - perm is the student's permanent address
; Interpretation: a Northeastern student

(define STUDENT-1 (make-student "Ryan" "Tietjen" 002936016 ADDRESS-1 ADDRESS-2))

#; (define (student-temp sd)
     (... (student-first sd) ... (student-last sd) ...
          (student-nuid sd) ... (student-local sd) ...
          (student-perm sd) ... ))


; TODO 1/3: complete the data design recipe for Address and NUStudent



; TODO 2/3: Design the function student-email which takes an NUStudent and
;           produces a string representing that student’s email address.
;           For simplicity we will say that a student’s email address is always
;           their last name (all lowercase),  followed by a period, followed
;           by the first initial of their first name (also lowercase; you can
;           assume this exists), and finished with "@northeastern.edu".

;; student-email: NUStudent -> String
;; returns a student's email
(check-expect (student-email STUDENT-1) "tietjen.r@northeastern.edu")

(define (student-email student)
  (string-append (string-downcase (student-last student)) "."
                 (substring (string-downcase (student-first student)) 0 1) "@northeastern.edu"))
  



; TODO 3/3: Design the function update-perm-zipcode which takes an NUStudent
;           and a natural number, representing the new zip code of the person,
;           and updates their permanent address to have that zip code.
;
;           Be sure to follow the template!

;; update-perm-zipcode : NUStudent Nat -> NUStudent
;; updates a student's permanent address to include their zip code

(check-expect (update-perm-zipcode STUDENT-1 00000)
              (make-student "Ryan" "Tietjen" 002936016 ADDRESS-1
                            (make-address 30 "Smith" "Freehold" "NJ" 00000)))

(define (update-perm-zipcode student zc)
  (make-student (student-first student) (student-last student)
                (student-nuid student) (student-local student)
                (make-address (address-num (student-perm student))
                              (address-st (student-perm student))
                              (address-city (student-perm student))
                              (address-us-state (student-perm student))
                              zc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note: to receive full credit, submit as much as you complete - you do NOT
;       have to finish all parts in lab.


; You are to design a program text-mover to display and manipulate text on a
; background. Your program should accept some phrase to show, as well as initial
; location and color (we only support three: red, black, or purple) - you should
; then display the phrase on the screen as described.

; When the user presses a mouse button, the program should move the text to the
; location that they clicked. When the user presses any key on the keyboard, the
; program should rotate colors.

; Here is our suggested plan for this program...

; 1. Design the text-mover function - think through the arguments to the
;    function, how you will represent the world state, and what handlers
;    you need to support.
;
;    - Hint A: since your state has multiple parts that change, you'll need a
;              structure to hold them, but the parts themselves might also be new.
;    - Hint B: you've been provided some data definitions below that will be quite
;              useful.

; 2. Finish designing the data from #1; think ahead to make examples that are
;    useful for testing such operations as changing location and color.

; 3. Design your to-draw handler, making use of the template(s) you 
;    designed in #2.

; 4. Design your remaining handler(s), again following the appropriate template(s).
;
;    - Hint #1: for the mouse, you'll want to respond only to the "button-up"
;               event, which you can check using the mouse=? function. Here's code
;               to get you started...

(define (mouse-handler state x y me)
  (if (mouse=? me "button-up")
      (make-tm (tm-str state) (make-posn x y) (tm-col state))
      state))

;    - Hint #2: make sure to follow your templates, which may involve breaking 
;               the handlers into helper functions.


; TODO 1/1: Design the text-mover World program!


; A Position is a (make-posn Real Real)
; Interpretation: a 2D location

(define POS-1 (make-posn 100 100))
(define POS-2 (make-posn 100 0))
(define POS-3 (make-posn -100 50))

(define (position-temp pos)
  (... (posn-x pos) ... (posn-y pos)))


; A RedBlackPurple (RBP) is one of:
; - "red"
; - "black"
; - "purple"
; Interpretation: available font colors

(define RBP-RED "red")
(define RBP-BLACK "black")
(define RBP-PURPLE "purple")

#;(define (red-black-purple-temp str)
    (cond [(string=? RBP-RED str) ...]
          [(string=? RBP-BLACK str) ...]
          [(string=? RBP-PURPLE str )...]))

(define-struct tm [str pos col])

; A TextMover (TM) is a (make-tm String Position RBP)
; - str is the text to be displayed
; - pos is the location of the text
; - col is the color of the text
; Interpretation: all the information needed for the text-mover program.

(define TM-1 (make-tm "hello world" (make-posn 250 250) "red"))
(define TM-2 (make-tm "kickrocks" (make-posn 175 200) "purple"))

(define (tm-temp tm)
  (... (tm-str tm) ...
       (tm-pos tm) ...
       (tm-col tm) ...))


(define BG (rectangle 1500 1000 "solid" "white"))


;; A TextState is a (make-tm String Position RBP)
;; representing the position, color, and text on a screen
(define TS-1 TM-1)
(define TS-2 TM-2)


;; text-mover: TextState -> TextState
;; displays and updates text on a screen
(define (text-mover tm)
  (big-bang tm
    [to-draw draw-text] ; TextState -> Image
    [on-key update-color] ; TextState KeyEvent -> TextState
    [on-mouse mouse-handler])) ; TextState -> TextState
    




;; draw-text: TextState -> Image
;; places text of a certain color on a blank screen
(check-expect (draw-text TS-1)
              (place-image (text "hello world" 50 "red")
                           250 250 BG))

(define (draw-text ts)
  (place-image (text (tm-str ts) 50 (tm-col ts))
               (posn-x (tm-pos ts)) (posn-y (tm-pos ts)) BG))


;; update-color TextState KeyEvent -> TextState
;; updates the color of the text whenever the user presses any key
(check-expect (update-color TS-1 "a")
              (make-tm (tm-str TS-1) (tm-pos TS-1) "black"))

(define (update-color ts ke)
  (make-tm (tm-str ts) (tm-pos ts) (change-color (tm-col ts)))) 
   
         
;; change-color: RBP -> RBP
;; rotates a color from red to black to purple.
(check-expect (change-color "red") "black")
(check-expect (change-color "black") "purple")
(check-expect (change-color "purple") "red")

(define (change-color rbp)
  (cond [(string=? rbp "red") "black"]
        [(string=? rbp "black") "purple"]
        [(string=? rbp "purple") "red"]))
