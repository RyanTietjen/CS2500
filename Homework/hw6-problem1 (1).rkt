;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw6-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In this problem you'll practice with lists by designing a function to
; help bookstores!

; Most bookstores sort the books on their shelves by the author’s last
; name. Unfortunately, some bookstore patrons do not preserve this order
; when browsing books, and simply place books back wherever they fit.

; Assume that bookstores keep all authors whose last name starts with the
; same letter on the same shelf, and those shelves are labeled with that
; letter. A record of which authors are on a given shelf would be represented
; using the following data definitions:

(define-struct shelf [letter authors])

; A Shelf is a (make-shelf 1String [List-of String])
; Interpretation: a record of the letter the authors' last name *should* start
; with, and the list of the *actual* last names on the shelf.

(define SHELF-1 (make-shelf "A" (list "Austen" "Hurston" "Angelou" "Butler" "Alvarez")))
(define SHELF-2 (make-shelf "B" (list)))
(define SHELF-3 (make-shelf "C" (list "Carle" "Coates")))
(define SHELF-4 (make-shelf "D" (list "Durston" "Dangelou" "Butler" "Alvarez")))

(define (shelf-temp s)
  (... (shelf-letter s) ...
       (list-of-string-temp (shelf-authors s)) ...))


; TODO 1/1: Design the function fix-shelves that takes a list of Shelf records
;           and produces a list of Shelf records where at least one author does
;           not belong on the Shelf. The output Shelf records should only contain
;           the authors who don't belong on that shelf. Shelf records and the authors
;           within those records should be in the same order in the output as they
;           appear in the input. Do not generate empty Shelf records; this generates
;           needlessly long reports, which annoys the employees. You have been
;           supplied a test for clarity (which you can use in your design, but
;           should supplement). Make sure your solution follows the (list) templates!


;; fix-shelves:  List-of Shelf -> List-of Shelf
;; interpretation: produces a list of Shelf records where at least one author
;; does not belong on the Shelf
(check-expect (fix-shelves (list SHELF-1 SHELF-2 SHELF-3))
              (list (make-shelf "A" (list "Hurston" "Butler"))))

(check-expect (fix-shelves (list SHELF-1 SHELF-2 SHELF-3 SHELF-4))
              (list (make-shelf "A" (list "Hurston" "Butler"))
                    (make-shelf "D" (list "Butler" "Alvarez"))))

(check-expect (fix-shelves (list SHELF-2 SHELF-3)) '())

(define (fix-shelves alos)
  (filter has-authors? (map keep-wrong-authors alos)))


;; has-authors? : Shelf -> Boolean
;; interpretation: determines if there are any authors in the shelf
(check-expect (has-authors? (keep-wrong-authors SHELF-1)) #true)
(check-expect (has-authors? (keep-wrong-authors SHELF-2)) #false)
(check-expect (has-authors? (keep-wrong-authors SHELF-3)) #false)
(check-expect (has-authors? (keep-wrong-authors SHELF-4)) #true)

(define (has-authors? s)
  (cons? (shelf-authors s)))

;; keep-wrong-authors: Shelf -> Shelf
;; interpretation: produces a Shelf record of all the authors that do not belong on a particular Shelf
(check-expect (keep-wrong-authors SHELF-1) (make-shelf "A" (list "Hurston" "Butler")))
(check-expect (keep-wrong-authors SHELF-2) (make-shelf "B" '()))
(check-expect (keep-wrong-authors SHELF-3) (make-shelf "C" '()))
(check-expect (keep-wrong-authors SHELF-4) (make-shelf "D" (list "Butler" "Alvarez")))

(define (keep-wrong-authors s)
  (make-shelf (shelf-letter s)
              (list-wrong-authors (shelf-authors s) (shelf-letter s))))


;; list-wrong-authors [List-of String] 1String -> [List-of String]
;; interpretation: produeces a list of authors whose name does not start with a given letter
(check-expect (list-wrong-authors (shelf-authors SHELF-1) "A") (list "Hurston" "Butler"))
(check-expect (list-wrong-authors (shelf-authors SHELF-2) "B") '())
(check-expect (list-wrong-authors (shelf-authors SHELF-3) "C") '())
(check-expect (list-wrong-authors (shelf-authors SHELF-4) "D") (list "Butler" "Alvarez"))

(define (list-wrong-authors alos str)
  (cond
    [(empty? alos) '()]
    [(cons? alos)
     (if (string=? str (substring (first alos) 0 1))
         (list-wrong-authors (rest alos) str)
         (cons (first alos) (list-wrong-authors (rest alos) str)))]))




