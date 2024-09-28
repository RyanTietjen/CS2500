;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Design the data SimpleExpression that allows you to represent
;           arithmetic expressions given the following requirements:
;           - There are two valid operations: add (+) and multiply (*)
;           - Each operation is applied to at least one operand, which may be a
;             number or another expression
;
;           Some example expressions include...
;            5
;            3.14 * 2
;            (3 + 4) * (1 + -2 + 7) * (5)
;            
;           Make sure to follow all steps of the design recipe for data and
;           define at least the examples above.

;; An Operation is one of
;; add (+)
;; multiply (*)
;; interpretation: an expression which may be a applied to at least one operand

(define ADD "+")
(define MULTIPLY "*")

(define (operation-temp op)
  (cond [(string=? op ADD) ...]
        [(string=? op MULTIPLY) ...]))


;; A SimpleExpression is one of:
;; - Real
;; (list Operation [List-of SimpleExpression]
;; interpretation: represents an arithmetric expression where + and * are the only
;; valid operations and each operation is applied to at least one operand,
;; which may be a number or another expression
(define SE-1 5)
(define SE-2 (list MULTIPLY 3.14 2))
(define SE-3 (list MULTIPLY (list ADD 3 4) (list ADD 1 -2 7) SE-1))

#;(define (se-temp se)
    (cond [(number? se) ...]
          [(cons? se) ...
           (operation-temp (first se)) ...
           (lose-temp (rest se)) ...]))

#;(define (lose-temp alose)
    (cond [(empty? alose) ...]
          [(cons? alose) ...
           (se-temp (first alose)) ...
           (lose-temp (rest alose))]))



; TODO 2/2: Now design the function evaluate that takes a SimpleExpression and
;           produces its numerical result. So for the examples above (where <=
;           means that the expression on the right evaluates to the value on the
;           left...
;
;            5    <= 5
;            6.28 <= 3.14 * 2
;            210  <= (3 + 4) * (1 + -2 + 7) * (5)

;; evaluate : SimpleExpression -> Real
;; takes a SimpleExpression and produces its numerical result
(check-expect (evaluate SE-1) 5)
(check-expect (evaluate SE-2) 6.28)
(check-expect (evaluate SE-3) 210)
(check-expect (evaluate (list ADD 3 4)) 7)

(define (evaluate se)
  (local [; perform-function : SimpleExpression -> SimpleExpression
          ; performs the operation in the SimpleExpression on all its operands 
          ; check SE-2, expect 6.28
          ; check SE-3, expect 210
          ; check (list ADD 3 4), expect 7
          (define (perform-function se)
            (if (number? se)
                se
                (if (string=? (first se) ADD)
                    (foldr + 0 (map perform-function (rest se)))
                    (foldr * 1 (map perform-function (rest se))))))]
    (cond [(number? se) se]
          [(cons? se) (perform-function se)])))