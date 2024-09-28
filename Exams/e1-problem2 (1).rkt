;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1-problem2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definition...


(define-struct puppy [name loud? rest])

; A DoggieParade is one of:
; - ":("
; - (make-puppy String Boolean DoggieParade)
; Interpretation: a sequence of doggies that is either empty (":(") or
; a puppy (with its name, whether it's loud, and the rest of the parade
; that follows.


; TODO 1/2: When you live in an apartment, you really can't have a loud
;           doggie parade. Now consider a function, apartment-friendly?,
;           that determines if the supplied parade of doggies lacks even
;           a single loud puppy. Your task: write a sufficient set of tests
;           for this function.
;
;           Notes:
;           - You do NOT actually have to design the function, but you can
;             if you wish (we'll ignore it).
;           - Assuming you do not write the function, we recommend that you
;             comment the check-expect statements for this problem, so that
;             they don't prevent you from running code in the next part of
;             this problem.

(define PARADE-1 ":(")
(define PARADE-2 (make-puppy "Jimmy" #false
                             (make-puppy "Oscar" #false
                                         (make-puppy "Casper" #false ":("))))
(define PARADE-3 (make-puppy "Greavys" #false
                             (make-puppy "Dean" #true
                                         (make-puppy "George" #false ":("))))
(define PARADE-4 (make-puppy "Sith" #true
                             (make-puppy "Mission" #true
                                         (make-puppy "Baba" #true
                                                     (make-puppy "Jaston" #true ":(")))))
  
;; apartment-friendly?: DoggieParade -> Boolean
;; Determines if a parade of dooggies lacks even a single loud puppy
(check-expect (apartment-friendly? PARADE-1) #true)
(check-expect (apartment-friendly? PARADE-2) #true)
(check-expect (apartment-friendly? PARADE-3) #false)
(check-expect (apartment-friendly? PARADE-4) #false)

(define (apartment-friendly? dp)
  (cond[(string? dp) #true]
       [(puppy? dp)
        (if (puppy-loud? dp)
            #false
            (apartment-friendly? (puppy-rest dp)))]))


; TODO 2/2: Design the function parade-names, which produces a textual
;           representation of the names of the puppies in the supplied
;           parade of doggies, with sadness at the end. For example,
;           an empty doggie parade would just be sad (":("), whereas
;           one that started with Bailey, followed by Spot, would be
;           "Bailey -> Spot -> :(".

;; parade-names: DoggieParade -> String
;; produces a textual representation of the name of the puppies in a parade
(check-expect (parade-names PARADE-1) ":(")
(check-expect (parade-names PARADE-2) "Jimmy -> Oscar -> Casper -> :(")
(check-expect (parade-names PARADE-3) "Greavys -> Dean -> George -> :(")
(check-expect (parade-names PARADE-4) "Sith -> Mission -> Baba -> Jaston -> :(")

(define (parade-names dp)
  (cond[(string? dp) ":("]
       [(puppy? dp)(string-append (puppy-name dp) " -> "
                                  (parade-names (puppy-rest dp)))]))