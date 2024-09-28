;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname e2-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/1: Design the data RoadNetwork to represent a network of roads in a new
;           town, which contains...

;           stretches of road of a particular distance (measured in kilometers,
;           or km)

;                 |   |
;                 | 3 |
;                 |   |
;                   ↑
;
;           (example stretch of road, 3km),

;           "dead ends", where roads end

;                 -----
;                 |   |
;                   ↑
;
;           (example dead end),

;           and "T" intersections, where a road can go left or right

;               -----------
;
;               ---┐   ┌---
;                  |   |
;                    ↑
;
;           (example T-intersection).

;           Notes:
;           - Your design does NOT need to in any way visualize the road
;             network, only represent any network made up of these road types.
;           - Remember to follow all steps of the design recipe for data!
;           - Your design MUST contain the following example (written
;             and then visualized for clarity; you may create more examples if
;             you'd like but they will not be graded)...

;             A stretch 3km long, arriving at a T intersection:
;             - to the left is a dead end.
;             - to the right is a stretch 1km long, followed by another stretch
;               7km long, arriving at a T-intersection:
;               - to the left is a stretch 4km long, followed by a dead end.
;               - to the the right is a stretch 2km long, followed by a
;                 T-intersection:
;                 - to the left is a stretch 2km long, followed by a dead end.
;                 - to the right is a stretch 3km long, followed by a dead end.
       
;                                ---
;                               |   |
;                               |   |
;                               | 4 |
;                               |   |
;                 --------------┘   |
;                |       1    7     |
;                 -┐   ┌--------┐   |
;                  |   |        | 2 |
;                  | 3 |        |   |
;                  |   |     ---┘   └--
;                    ↑      | 3      2 |
;                            ----------


(define-struct stretch [length road])
(define-struct t [left right])

; A Stretch is a (make-stretch Nat RoadNetwork)
; interepretation: A stretch of road of a particular length that continues into a network of roads

(define STRETCH-1 (make-stretch 4 #f))
(define STRETCH-2 (make-stretch 2 (make-t (make-stretch 3 #f) (make-stretch 2 #f))))
(define STRETCH-3 (make-stretch 7 (make-t STRETCH-1 STRETCH-2)))
(define STRETCH-4 (make-stretch 1 STRETCH-3))
(define STRETCH-5 (make-stretch 3 (make-t #f STRETCH-4)))

(define (strech-temp len r)
  (... (stretch-length len) ...
       (road-network-temp (stretch-road r)) ...))

; A T is a (make-t RoadNetwork RoadNetwork)
; interepretation: A "T" intersection, where a road can go left or right

(define T-1 (make-t (make-stretch 3 #f) (make-stretch 2 #f)))
(define T-2 (make-t STRETCH-1 STRETCH-2))
(define T-3 (make-t #f STRETCH-4))
  
(define (t-temp l r)
  (... (road-network-temp (t-left l)) ...
       (road-network-temp (t-right r)) ...))

; A RoadNetwork is one of
; #f
; Stretch
; T
; interpretation: a network of roads in a new town, where #f represents a dead end

;; example required:
(define ROAD-NETWORK-1 STRETCH-1)

(define ROAD-NETWORK-2 T-2)
(define ROAD-NETWORK-3 STRETCH-3)

(define (road-network-temp rn)
  (cond [(false? rn) ...]
        [(stretch? rn) (... (stretch-length rn) ...
                            (road-network-temp (stretch-road rn)) ...)]
        [(t? rn) (... (road-network-temp (t-left rn)) ...
                      (road-network-temp (t-right rn)) ...)]))