;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For any word of at least one character that starts with a letter,
; let’s say that its "bingo word" is the uppercase version of the
; first letter, followed by a space, and then followed by the number
; of characters in the word. For example, the bingo word of "bingo"
; is "B 5" and the bingo word of "Win" is "W 3".

; TODO 1/1: Define a function, bingo-word, that takes a string as an argument
;           and returns its bingo word. You may assume that the argument is a
;           valid word as described above.
;
;           Don't forget to include a signature and reasonable purpose statement!!
;
;           Hint: if you don't remember ALL the string functions in BSL, that's
;           ok!! :) Remember that you can right-click on a function and search
;           the Help Desk - as a start, string-append will be quite handy...
;           and now you just need some help with isolating substrings,
;           converting strings to upper case, getting the length of a string, and
;           converting a number to a string. Good luck!!


; bingo-word: String -> String
; returns the bingo word of a given string  
(check-expect (bingo-word "bingo") "B 5")
(check-expect (bingo-word "win") "W 3")
(define (bingo-word word)
  (string-append (string-upcase(substring word 0 1)) " " (number->string (string-length word))))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's make a pretty animated scene with a house, in parts!

; TODO 1/4: Use the triangle, square, rectangle, above, and overlay/align
;           functions to define a constant HOUSE that is the image of a
;           house with a roof and door (and circle if you’re feeling b=old
;           enough for a door handle). Be creative :)

(define BASE (square 200 "solid" "yellow"))
(define ROOF (triangle 200 "solid" "grey"))
(define DOOR (rectangle 55 120 "solid" "brown"))

(define HOME (overlay/align "middle" "bottom" DOOR (above ROOF BASE)))


; TODO 2/4: Define a constant WINDOW, as the image of a window, and place
;           two of them on your HOME, defining HOUSE-WITH-WINDOWS.
;           Note how in using a constant we only have to draw it once and
;           get to use it twice!

(define WINDOW (square 40 "solid" "white"))

(define TEMP-HOUSE (place-image WINDOW 35 290 HOME))
(define HOUSE-WITH-WINDOWS (place-image WINDOW 165 290 TEMP-HOUSE))


  ; The next step is a bit tricky, and will require you to understand a bit
  ; about how colors are represented in DrRacket (and other languages!).

  ; Colors in DrRacket can either defined via a name (like "blue" and "red"),
  ; or by numbers, representing the amount of red, green, and blue (each a
  ; number from 0-255) using the color function...

  ; (color red-val green-val blue-val)

  ; For example, a bright red square could be created as either of the following...

  ; (square 100 "solid" (color 255 0 0))
  ; (square 100 "solid" "red")

  ; Now consider the following function, which uses a mathematical formula to
  ; produce a range of blues...

  ; Nat -> color
  (define (sky-color t)
    (color 0 0 (abs (- (remainder t 510) 255))))

  ; This function always uses 0's for red and green, but differs in the amount
  ; of blue. If it helps, here is an infix representation of the equation...

  ; |(t remainder 510) - 255|

  ; and here is a visual depiction of how the amount of blue changes as a function
  ; of the value of t: https://www.desmos.com/calculator/ntq43wwjpg

  ; As you can see, the amount of blue moves linearly up and down between 0 and 255.

  ; TODO 3/4: Replace "SIGNATURE HERE" above with a signature for this function.
  ;           Note: ordinarily we'd also have a purpose statement... but that's
  ;           kinda what this whole set of comments was about ;)


  ; TODO 4/4: Now finally we are ready to put it all together :)
  ;           The goal is that when you uncomment the final line below, you will see
  ;           a movie of your house, with the sky getting darker and then lighter
  ;           behind it. SO, define a function scene that uses your HOUSE-WITH-WINDOWS
  ;           constant, as well as the sky-color function, to visualize a single
  ;           frame of the movie, where the frame number determines the sky color,
  ;           and so...

  (define (scene a)
    (underlay/align "middle" "middle" (square 400 "solid" (sky-color a)) HOUSE-WITH-WINDOWS))
  ;
   (scene 0)
  ;
  ;           should have your house in front of a bright blue background, whereas...
  ;
   (scene 255)
  ;           
  ;           should have your house in front of a black background (night!).

  ; scene : Nat -> Image
  ; visualizes a house where the supplied number determines
  ; the amount of blue in the sky behind the house


   (animate scene)
  