;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname e1-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Your team has been tasked with building the next-generation app for Delta
; Air Lines. As a part of this massive undertaking, your job is to design
; the data to represent a single row on an international Delta flight.

; TODO 1/1: Design the data Row that has a number (e.g., 33), a passenger
;           class (either "main", "comfort+", "premium select", or
;           "delta one"), and the designation as an exit row or not.
;
;           Ensure you follow the design recipe for data!


;; A Class is one of
;; - "main"
;; - "comfort+"
;; - "premium select"
;; - "delta one"
;; Interpretation: The passenger class of a particular row
;; on a Delta Air Lines flight
(define CLASS-MAIN "main")
(define CLASS-COMFORT "comfort+")
(define CLASS-PREMIUM "premium select")
(define CLASS-DELTA "delta one")

(define (class-temp c)
  (...
   (cond[(string=? c CLASS-MAIN) ...]
        [(string=? c CLASS-COMFORT) ...]
        [(string=? c CLASS-PREMIUM) ...]
        [(string=? c CLASS-DELTA) ...])))


(define-struct row [number class exit])

;; A row is a (make-row PosInt Class Boolean)
;; number is the row number
;; class is the pasenger class of the row
;; exit is if the row is designated as an exit row
;; Interpretation: Information regarding a row on a Delta Air Lines flight
(define ROW-1 (make-row 33 CLASS-MAIN #true))
(define ROW-2 (make-row 20 CLASS-COMFORT #true))
(define ROW-3 (make-row 8 CLASS-PREMIUM #false))
(define ROW-4 (make-row 17 CLASS-DELTA #false))

(define (row-temp r)
  (... (row-number r) ...
       (class-temp (row-class r)) ...
       (row-exit r) ...))