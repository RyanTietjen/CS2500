;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw5-problem1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You are going to make yourself a useful interactive app: flash cards
; (https://en.wikipedia.org/wiki/Flashcard).

; To begin, consider the following data definition...


(define-struct flashcard [front back])

; A FlashCard is a (make-flashcard String String)
; Interpretation: the front and back of a card


; TODO 1/4: Complete the design recipe for FlashCard.

(define FLASHCARD-1 (make-flashcard "Hello ...?" "Hello World"))
(define FLASHCARD-2 (make-flashcard "Boolean" "A value that is either true or false"))
(define FLASHCARD-3 (make-flashcard "HTDP" "How to design programming"))

(define (flashcard-temp fc)
  (... (flashcard-front fc) ... (flashcard-back fc) ...))


; Now a single flash card wouldn't be super useful, and so...

; TODO 2/4: Design ListOfFlashCard (LoFC) to support an arbitrarily sized
;           sequence of flash cards. Importantly...
;           - These should be proper lists (i.e., using cons and '()).
;           - Make sure to give yourself a few example lists, of different sizes;
;             hopefully they are useful in your classes!
;           - Remember that your LoFC template should reflect that your list
;             elements are themselves designed types (FlashCard).

;; A ListofFlashCard (LoFC) is one of:
;; - '()
;; (cons FlashCard LoFC)
;; Interpretation: represents a list of flash cards
(define LOFC-1 '())
(define LOFC-2 (cons FLASHCARD-1 '()))
(define LOFC-3 (cons FLASHCARD-2 LOFC-2))
(define LOFC-4 (cons FLASHCARD-3 LOFC-3))

#;(define (lofc-temp alofc)
    (cond [(empty? alofc) ...]
          [(cons? alofc) ... (first alofc) ...
                         ... (lofc-temp (rest alofc)) ...]))




; Now, for practice...

; TODO 3/4: Design the function has-text?, which determines if a list of flash
;           cards contains any card that contains a supplied text.
;
;           Hint: the string-contains? function is very useful for determining
;           if one string contains another :)

;; define has-text? : LoFC String -> Boolean
;; determines if any card contains a supplied text
(check-expect (has-text? LOFC-1 "anything") #false)
(check-expect (has-text? LOFC-4 "anything") #false)
(check-expect (has-text? LOFC-2 "Boolean") #false)
(check-expect (has-text? LOFC-3 "Boolean") #true)
(check-expect (has-text? LOFC-4 "design") #true)

(define (has-text? alofc str)
  (cond [(empty? alofc) #false]
        [(cons? alofc)
         (if (or (string-contains? str (flashcard-front (first alofc)))
                 (string-contains? str (flashcard-back (first alofc))))
             #true
             (has-text? (rest alofc) str))]))





; Finally, let's put this list to use :)

; TODO 4/4: Design the program go-cards, which helps you study with a supplied list
;           of cards. It starts on the first card and then flips it when a key is
;           pressed, and then goes to the front of the next card when another key is
;           pressed. The program should end when the last card has been flipped, and
;           the go-cards function should return how many cards were in the original
;           list. Some hints...
;           - To get you started, you have been supplied the data definition of a
;             way to represent the state of the program (don't forget to uncomment
;             the structure definition and finish the design recipe for data!).
;           - The return value of this function is a bit challenging, since the list
;             you get at the end is empty! So uncomment the code we've given you below,
;             but to understand: you can *add* the length of the originally supplied
;             list to that of the (empty) final list and still get the right answer :)
;           - Be sure to follow the templates for all your data, which will typically
;             entail helpers for the FS, the LoFC, and the FC.
;           - As long as the program operates as described, you are welcome to make it
;             look as simple or as creative as you would like - we hope it helps you
;             in your classes!! :)


(define-struct fs [cards front?])

; A FlashState (FS) is a (make-fs LoFC Boolean)
; Interpretation: a list of cards, and whether
; the front is face up
(define FS-1 (make-fs LOFC-1 #true))
(define FS-2 (make-fs LOFC-4 #true))
(define FS-3 (make-fs LOFC-4 #false))
(define FS-4 (make-fs LOFC-3 #true))
(define FS-5 (make-fs LOFC-3 #false))
(define FS-6 (make-fs LOFC-2 #true))
(define FS-7 (make-fs LOFC-2 #false))

(define (fs-temp fs)
  (... (lofc-temp (fs-cards fs)) ...
       ... (fs-front? fs)))
   


; go-cards : LoFC -> Nat
; displays the cards in sequence (flip via key),
; returning the number of cards
(define (go-cards lofc)
  (+
   (length lofc)
   (length (fs-cards
            (big-bang (make-fs lofc #t)
              [to-draw draw-fs]
              [on-key flip-fs]
              [stop-when done-fs?])))))


;; draw-card: FlashState -> Image
;; interpretatoin: places the text of a flashcard onto a blank background
(check-expect (draw-fs FS-1) (empty-scene 400 400))
(check-expect (draw-fs FS-5) (overlay/align "middle" "middle"
                                            (text "A value that is either true or false" 24 "black")
                                            (empty-scene 400 400)))
(check-expect (draw-fs FS-2) (overlay/align "middle" "middle"
                                            (text "HTDP" 24 "black")
                                            (empty-scene 400 400)))

(define (draw-fs fs)
  (cond [(empty? (fs-cards fs)) (empty-scene 400 400)]
        [(cons? (fs-cards fs))
         (if (fs-front? fs)
             (overlay/align "middle" "middle"
                            (text (flashcard-front (first (fs-cards fs))) 24 "black")
                            (empty-scene 400 400))
             (overlay/align "middle" "middle"
                            (text (flashcard-back (first (fs-cards fs))) 24 "black")
                            (empty-scene 400 400)))]))

;; flip-fs: FlashState KeyEvent -> FlashState
;; intepretation: flips a card if the front of the card is showing,
;; otherwise it moves on to the next card
(check-expect (flip-fs FS-6 "a") FS-7)
(check-expect (flip-fs FS-7 "z") FS-1)

(define (flip-fs fs ke)
  (if (fs-front? fs)
      (make-fs (fs-cards fs) #false)
      (make-fs (rest (fs-cards fs)) #true)))

;; done-fs? : FlashState -> Boolean
;; interpretation: determines if there are no more cards in the List of Flashcards
(check-expect (done-fs? FS-7) #false)
(check-expect (done-fs? FS-1) #true)

(define (done-fs? fs)
  (empty? (fs-cards fs)))

