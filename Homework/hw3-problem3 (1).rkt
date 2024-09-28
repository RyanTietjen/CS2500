;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw3-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Recently a new website (https://neal.fun/design-the-next-iphone/) was made
; to allow you to design the next iPhone by mixing & matching features.
; Let's make a simple version of this as a World program :)

; TODO 1/3: Design the data PhoneModel that allows you to represent a set
;           of (at least) four enumerated models. You are welcome to be
;           creative, but an example set of options could be "Small",
;           "Regular", "Max", or "Fold"

; A PhoneModel is one of:
; - "Small"
; - "Regular"
; - "Max"
; - "Fold"
; Interpretation: The potential sizes of a phone model

(define PM-SMALL "Small")
(define PM-REGULAR "Regular")
(define PM-MAX "Max")
(define PM-FOLD "Fold")

(define (pm-temp str)
  (cond [(string=? str PM-SMALL) ...]
        [(string=? str PM-REGULAR) ...]
        [(string=? str PM-MAX) ...]
        [(string=? str PM-FOLD) ...]))


; TODO 2/3: Now design the data PhoneCompany that allows you to represent a set
;           of (at least) three enumerated company names, such as "Apple",
;           "Google", "Amazon".

; A PhoneCompany is one of:
; - "Apple"
; - "Google"
; - "Amazon"
; Interpretation: The potential company of the phone

(define PC-APPLE "Apple")
(define PC-GOOGLE "Google")
(define PC-AMAZON "Amazon")

(define (pc-temp str)
  (cond [(string=? str PC-APPLE) ...]
        [(string=? str PC-GOOGLE) ...]
        [(string=? str PC-AMAZON) ...]))


; TODO 3/3: Now design a World program that allows someone to interactively see all
;           combinations of models and companies.
;
;           Notes:
;           - You will need to design data to represent the current combination of
;             model/company, which should be a structure. Your program can accept
;             the starting value and should return the last when the window is closed.
;           - When the "m" keyboard key is pressed, you should proceed to the next
;             model (e.g., "Small" -> "Regular" -> "Max" -> "Fold" -> "Small"...);
;             the "c" key should similarly work for companies.
;           - Your drawing function should overlay some visualization of the company
;             (e.g., could be just the text, or a pretty logo) on top of a
;             visualization of the model (e.g., could differ in size, camera, ...).
;           - Be sure to follow the templates, which will guide you as to when to
;             create data-specific helper functions!!


(define BG (rectangle 1000 1000 "solid" "white")) 
(define PHONE-SMALL (overlay/align "middle" "middle"
                                   (overlay/align "middle" "bottom"
                                                  (circle 15 "solid" "white")
                                                  (rectangle 100 200 "solid" "grey"))
                                   (rectangle 120 220 "solid" "black")))
(define PHONE-REGULAR (overlay/align "middle" "middle"
                                     (overlay/align "middle" "bottom"
                                                    (circle 22.5 "solid" "white")
                                                    (rectangle 150 300 "solid" "grey"))
                                     (rectangle 180 330 "solid" "black")))
(define PHONE-MAX (overlay/align "middle" "middle"
                                 (overlay/align "middle" "bottom"
                                                (circle 45 "solid" "white")
                                                (rectangle 300 600 "solid" "grey"))
                                 (rectangle 360 660 "solid" "black")))
(define PHONE-FOLD (overlay/align "middle" "middle"
                                  (overlay/align "middle" "bottom"
                                                 (circle 15 "solid" "white")
                                                 (rectangle 100 100 "solid" "grey"))
                                  (rectangle 120 120 "solid" "black")))

(define-struct phone [pm pc])

; A Phone is a (make-phone PhoneModel PhoneCompany)
; Interpretation: A phone with a certain model size and manufacturing company

(define PHONE-SM-AP (make-phone PM-SMALL PC-APPLE))
(define PHONE-SM-GO (make-phone PM-SMALL PC-GOOGLE))
(define PHONE-SM-AM (make-phone PM-SMALL PC-AMAZON))

(define PHONE-REG-AP (make-phone PM-REGULAR PC-APPLE))
(define PHONE-REG-GO (make-phone PM-REGULAR PC-GOOGLE))
(define PHONE-REG-AM (make-phone PM-REGULAR PC-AMAZON))

(define PHONE-MAX-AP (make-phone PM-MAX PC-APPLE))
(define PHONE-MAX-GO (make-phone PM-MAX PC-GOOGLE))
(define PHONE-MAX-AM (make-phone PM-MAX PC-AMAZON))

(define PHONE-FOLD-AP (make-phone PM-FOLD PC-APPLE))
(define PHONE-FOLD-GO (make-phone PM-FOLD PC-GOOGLE))
(define PHONE-FOLD-AM (make-phone PM-FOLD PC-AMAZON))

(define (phone-temp phone)
  (... (pm-temp (phone-pm phone)) ...
       (pc-temp (phone-pc phone) ...)))

; make-phone PhoneModel PhoneCompany
; phone? Any -> Boolean
; phone-temp: Phone -> ?
; phone-pm: Phone -> PhoneModel
; phone-pc: Phone -> PhoneCompany


; A PhoneState is a (make-phone PhoneModel PhoneCompany)
; representing a phone with a particular model size and manufacturing company
(define PS-1 PHONE-SM-AP)
(define PS-2 PHONE-REG-GO)
(define PS-3 PHONE-MAX-AM)
(define PS-4 PHONE-FOLD-AP)

; phone-sim : PhoneState -> PhoneState 
(define (phone-sim ps)
  (big-bang ps
    [to-draw draw-sim] ; PhoneState -> Image
    [on-key update-sim])) ; PhoneState KeyEvent -> PhoneState

; draw-sim: PhoneState -> Image
; draws an image of a phone, with a certain size and brand, given the current PhoneState
(check-expect (draw-sim PS-1) (overlay/align "middle" "middle"
                                             (above/align "middle"
                                                          (text "Apple" 50 "Black")
                                                          PHONE-SMALL) BG))
(check-expect (draw-sim PS-2) (overlay/align "middle" "middle"
                                             (above/align "middle"
                                                          (text "Google" 50 "Black")
                                                          PHONE-REGULAR) BG))
(check-expect (draw-sim PS-3) (overlay/align "middle" "middle"
                                             (above/align "middle"
                                                          (text "Amazon" 50 "Black")
                                                          PHONE-MAX) BG))
(check-expect (draw-sim PS-4) (overlay/align "middle" "middle"
                                             (above/align "middle"
                                                          (text "Apple" 50 "Black")
                                                          PHONE-FOLD) BG))

(define (draw-sim ps)
  (overlay/align "middle" "middle"
                 (above/align "middle"
                              (cond [(string=? PC-APPLE (phone-pc ps)) (text "Apple" 50 "Black")]
                                    [(string=? PC-GOOGLE (phone-pc ps)) (text "Google" 50 "Black")]
                                    [(string=? PC-AMAZON (phone-pc ps)) (text "Amazon" 50 "Black")])
                              (cond [(string=? PM-SMALL (phone-pm ps)) PHONE-SMALL]
                                    [(string=? PM-REGULAR (phone-pm ps)) PHONE-REGULAR]
                                    [(string=? PM-MAX (phone-pm ps)) PHONE-MAX]
                                    [(string=? PM-FOLD (phone-pm ps)) PHONE-FOLD])) BG))


; update-sim : PhoneState KeyEvent -> PhoneState
; updates the size of the phone sim when "m" is pressed and updates
; the brand of the phone when "c" is pressed
(check-expect (update-sim PS-1 "c") (make-phone PM-SMALL PC-GOOGLE))
(check-expect (update-sim PS-2 "c") (make-phone PM-REGULAR PC-AMAZON))
(check-expect (update-sim PS-3 "c") (make-phone PM-MAX PC-APPLE))
(check-expect (update-sim PS-4 "m") (make-phone PM-SMALL PC-APPLE))
(check-expect (update-sim PS-1 "m") (make-phone PM-REGULAR PC-APPLE))
(check-expect (update-sim PS-2 "m") (make-phone PM-MAX PC-GOOGLE))
(check-expect (update-sim PS-3 "m") (make-phone PM-FOLD PC-AMAZON))
(check-expect (update-sim PS-1 "s") PS-1)

(define (update-sim ps ke)
  (cond [(string=? ke "m")
         (cond [(string=? PM-SMALL (phone-pm ps)) (make-phone PM-REGULAR (phone-pc ps))]
               [(string=? PM-REGULAR (phone-pm ps)) (make-phone PM-MAX (phone-pc ps))]
               [(string=? PM-MAX (phone-pm ps)) (make-phone PM-FOLD (phone-pc ps))]
               [(string=? PM-FOLD (phone-pm ps)) (make-phone PM-SMALL (phone-pc ps))])]
        [(string=? ke "c")
         (cond [(string=? PC-APPLE (phone-pc ps)) (make-phone (phone-pm ps) PC-GOOGLE)]
               [(string=? PC-GOOGLE (phone-pc ps)) (make-phone (phone-pm ps) PC-AMAZON)]
               [(string=? PC-AMAZON (phone-pc ps)) (make-phone (phone-pm ps) PC-APPLE)])]
        [else ps]))
