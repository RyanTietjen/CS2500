;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-problem3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now let's think about what goes into making an app that works with purchase
; receipts.
;
; Consider the following data definition:


(define-struct item [desc qty unit sale? next])

; A ReceiptItem is one of:
; - "nothing"
; - (make-item String Nat PosReal Boolean ReceiptItem)
; Intepretation: either end of the receipt or
; an item's description, quantity purchased,
; unit price (in $), whether it was on sale,
; and the next item on the receipt
; - desc is the description of the item purchased
; - qty is the number items purchased
; - unit is the cost of the item
; - sale? is if the item was on sale
; - next is the next ReciptItem on the list


; TODO 1/4: Complete the data design recipe for ReceiptItem.
;           You *must* have examples that (at least) represent the following
;           three receipts...
;           - An empty receipt
;           - A grocery receipt...
;             (1 box of cereal, $4.28),
;             (2 apples on sale, $1.67 each)
;           - A computer invoice...
;             (2 RaspberryPi on sale, $32 each),
;             (1 monitor, $135),
;             (2 wireless touch keyboards, $27 each)

(define RECEIPT-1 "nothing")
(define RECEIPT-2 (make-item "box of cereal" 1 4.28 #false
                             (make-item "apple" 2 1.67 #true "nothing")))
(define RECEIPT-3 (make-item "Raspberry Pi" 2 32 #true
                             (make-item "monitor" 1 135 #false
                                        (make-item "wireless touch keyboard" 2 27 #false "nothing"))))

(define (item-temp ri)
  (...
   (cond [(string? ri) ...]
         [(item? ri)
          (... (item-desc ri) ...
               (item-qty ri) ...
               (item-unit ri) ...
               (item-sale? ri) ...
               (item-temp (item-next ri)) ...)])))
         

 


; TODO 2/4: Design the function total-cost, which calculates the total cost
;           of a receipt. For instance, the empty receipt is 0; the grocery
;           is (1 x 4.28) + (2 x 1.67) = 7.62; and the computer receipt is
;           (2 x 32) + (1 x 135) + (2 x 27) = 253.

; total-cost: ReceiptItem -> PosReal
; computes the total cost of a recipt
(check-expect (total-cost RECEIPT-1) 0)
(check-expect (total-cost RECEIPT-2) 7.62)
(check-expect (total-cost RECEIPT-3) 253)

(define (total-cost ri)
  (cond [(string? ri) 0]
        [(item? ri)
         (+ (* (item-qty ri) (item-unit ri))
            (total-cost (item-next ri)))]))




; TODO 3/4: Design the function any-sale?, which determines if any item in the
;           receipt is on sale. For example, the empty receipt does not have
;           any sale items, but both other examples do.

; any-sale? ReceiptItem -> Boolean
; determines if any item in the recipt is on sale
(check-expect (any-sale? RECEIPT-1) #false)
(check-expect (any-sale? RECEIPT-2) #true)
(check-expect (any-sale? RECEIPT-3) #true)

(define (any-sale? ri)
  (cond [(string? ri) #false]
        [(item? ri)
         (if (item-sale? ri) #true
             (any-sale? (item-next ri)))]))



; TODO 4/4: Design the function expensive, which produces a new receipt that only
;           contains items that are greater than $100 (unit cost). For example,
;           both the empty and grocery receipts would produce empty receipts,
;           whereas the computer receipt would produce a new list only containing
;           the monitor.

; expensive: ReceiptItem -> ReceiptItem
; produces a new receipt only containing items greater than $100 given any receipt
(check-expect (expensive RECEIPT-1) "nothing")
(check-expect (expensive RECEIPT-2) "nothing")
(check-expect (expensive RECEIPT-3) (make-item "monitor" 1 135 #false "nothing"))

(define (expensive ri)
  (cond [(string? ri) "nothing"]
        [(item? ri)
         (if (> (item-unit ri) 100)
             (make-item (item-desc ri) (item-qty ri) (item-unit ri) (item-sale? ri)
                        (expensive (item-next ri)))
             (expensive (item-next ri)))]))