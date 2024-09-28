;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-p2done) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's think about what goes into making a particularly useful app, Weather!
;
; Consider the following data definition:


;(define-struct cloudy [morn? eve?])
;(define-struct rain [chance])
;(define-struct snow [inches])

; A Prediction is one of:
; - "sunny"
; - (make-cloudy Boolean Boolean)
; - (make-rain Nat[1, 100])
; - (make-snow Nat)
; Interpretation: weather prediction, either...
; - Sunny!
; - Cloudy (either in the morning, evening, both, or unsure)
; - Raining (with provided % chance as 1-100)
; - Snow (with provided accumulation)


; TODO 1/2: Complete the data design recipe for Prediction.

(define-struct cloudy [morn? eve?])
; A Cloudy is a (make-cloudy Boolean Boolean)
; Interpretation: a forecast for cloudy skies either
; in the morning, evening, both, or an undetermined time
; morn? is #true if it will be cloudy in the morning, #false if not
; eve? is #true if it will be cloudy in the evening, #false if not

(define CLOUDY-0 (make-cloudy #true #true))
(define CLOUDY-1 (make-cloudy #false #true))
(define CLOUDY-2 (make-cloudy #true #false))
(define CLOUDY-3 (make-cloudy #false #false))

(define (cloudy-temp cloudy)
  (...
   (cloudy-morn? cloudy) ...
   (cloudy-eve? cloudy) ...))
     
(define-struct rain [chance])
; A Raining is a (make-rain Nat[1, 100])
; Interpretation: a percent chance of rain between 1 and 100
; chance is the percent chance of rain

(define RAIN-0 (make-rain 0))
(define RAIN-1 (make-rain 30))
(define RAIN-2 (make-rain 75))
(define RAIN-3 (make-rain 100))

(define (rain-temp rain)
  (... (rain-chance rain) ...))

(define-struct snow [inches])
; A Snow is a (make-snow Nat)
; Interpretation: a predicted amount of snow
; inches is the number of inches of snow

(define SNOW-0 (make-snow 0))
(define SNOW-1 (make-snow 3))
(define SNOW-2 (make-snow 7))
(define SNOW-3 (make-snow 13))

(define (snow-temp snow)
  (... (snow-inches snow) ...))

; A Prediction is one of:
; - "sunny"
; - (make-cloudy Boolean Boolean)
; - (make-rain Nat[1, 100])
; - (make-snow Nat)
; Interpretation: weather prediction, either...
; - Sunny!
; - Cloudy (either in the morning, evening, both, or unsure)
; - Raining (with provided % chance as 1-100)
; - Snow (with provided accumulation)

(define PRED-0 CLOUDY-1)
(define PRED-1 RAIN-2)
(define PRED-2 SNOW-2)
(define PRED-3 CLOUDY-3)
(define PRED-4 "sunny")
  
(define (pred-temp pred)
  (... 
   (cond [(string? pred) ...]
         [(cloudy? pred) ... (info-temp) ...]
         [(rain? pred) ... (badge-temp) ...]
         [(snow? pred) ... (confirmation-temp) ...])))


; TODO 2/2: Design the function announcement, which given
;           a prediction (e.g., "sunny"), produces a short
;           text announcement to display (e.g., "It's going
;           to be sunny!").

;           Some other example announcements include:
;           - "It's going to be cloudy in the morning."
;           - "There's a 60% chance of rain."
;           - "It's going to snow, with 2 inches on the ground."

; announcement : Prediction -> Image
; makes an image displaying a message describing the weather forecast

(check-expect (announcement PRED-0) (text "It's going to be cloudy tonight." 20 "blue"))
(check-expect (announcement PRED-1) (text "There's a 75% chance of rain." 20 "yellow"))
(check-expect (announcement PRED-2) (text "There's going to be 7 inches of snow!" 20 "red"))
(check-expect (announcement PRED-3) (text "It's going to be cloudy sometime today." 20 "blue"))
(check-expect (announcement PRED-4) (text "It's going to be sunny!" 20 "green"))

(define (announcement pred)
  (cond [(string? pred) (text "It's going to be sunny!" 20 "green")]
        [(cloudy? pred) (text (string-append "It's going to be cloudy "
                                             (when-cloudy pred) ".") 20 "blue")]
        [(rain? pred) (text (string-append "There's a "
                                           (number->string
                                            (rain-chance pred)) "% chance of rain.") 20 "yellow")]
        [(snow? pred) (text (string-append "There's going to be "
                                           (number->string
                                            (snow-inches pred)) " inches of snow!") 20 "red")]))

; when-cloudy : Cloudy -> String
; says the time of day when rain will fall

(check-expect (when-cloudy CLOUDY-0) "all day")
(check-expect (when-cloudy CLOUDY-1) "tonight")
(check-expect (when-cloudy CLOUDY-2) "this morning")
(check-expect (when-cloudy CLOUDY-3) "sometime today")

(define (when-cloudy cloudy)
  (cond [(false? (cloudy-morn? cloudy))
         (if (boolean=? (cloudy-morn? cloudy) (cloudy-eve? cloudy)) "sometime today"
             "tonight")]   
        [(boolean=? (cloudy-morn? cloudy) (cloudy-eve? cloudy)) "all day"]
        [else "this morning"]))
 




