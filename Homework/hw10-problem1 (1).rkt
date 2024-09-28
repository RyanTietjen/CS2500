;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw10-problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Finish designing the function countdown that takes a natural number
;           and a non-empty list of strings and produces a countdown message.
;
;           Note: you are NOT allowed to use list abstractions for this
;                 function; doing so will result in ZERO code credit :(


; countdown : Nat [NEList-of String] -> String
; produces a message of counting down the numbers and then
; listing out the messages


(check-expect (countdown 3 (list "go")) "3!2!1!go!")
(check-expect (countdown 0 (list "howdy")) "howdy!")
(check-expect (countdown 2 (list "check" "expect")) "2!1!check!expect!")

#;(define (countdown num alos)
    (local[(define NUMBER-STRING
             (foldl string-append ""
                    (append (build-list num (λ (n) (string-append  (number->string (add1 n)) "!"))))))
           (define WORD-STRING
             (foldl string-append ""
                    (reverse (map (λ (str) (string-append str "!")) alos))))]
      (string-append NUMBER-STRING WORD-STRING)))


(define (countdown num alos)
  (local [; number-creator : Number -> String
          ; creates a string counting down numbers
          ; check - 3, expect - "3!2!1!"
          ; check - 0, expect - ""
          (define (number-creator n)
            (if (= n 0)
                ""
                (string-append (string-append (number->string n) "!") (number-creator (- n 1)))))
          (define NUMBER-STRING  (number-creator num))
          ; word-creator : [List-of String] -> String
          ; creates a string listing out the messages
          ; check - (list "go"), expect - "go!"
          ; check - (list "check" "expect"), expect - "check!expect!"
          (define (word-creator alos)
            (cond [(empty? alos) ""]
                  [(cons? alos)
                   (string-append (string-append (first alos) "!") (word-creator (rest alos)))]))
          (define WORD-STRING (word-creator alos))]
    (string-append NUMBER-STRING WORD-STRING)))
    



; TODO 2/2: Finish designing the function lists-same? that determines if two
;           supplied lists have the "same" contents, as determined by a supplied
;           predicate.
;
;           Note: you are NOT allowed to use list abstractions for this
;                 function; doing so will result in ZERO code credit :(


; lists-same? : (X Y) [List-of X] [List-of Y] [X Y -> Boolean] -> Boolean
; determines if the two lists are the "same" based upon the predicate


(check-expect (lists-same? '() '() =) #t)
(check-expect (lists-same? '() (list 1) =) #f)
(check-expect (lists-same? (list 1 2) '() =) #f)

(check-expect (lists-same? (list 1) (list 1) =) #t)
(check-expect (lists-same? (list 1) (list 100) =) #f)
(check-expect (lists-same? (list 1 100) (list 1 2) =) #f)
(check-expect (lists-same? (list "a" "b" "c") (list "a" "b" "c") string=?) #t)
(check-expect (lists-same? (list "a" "b" "c") (list "c" "b" "a") string=?) #f)
(check-expect (lists-same? (list "a" "b" "c") (list "a" "b" "a") string=?) #f)

(check-expect (lists-same? (list 1 2 3) (list "1" "2" "3")
                           (λ (x y) (string=? (number->string x) y))) #t)

(check-expect (lists-same? (list "howdy" "world") (list "Howdy" "WORLD")
                           (λ (x y) (string=? (string-upcase x)
                                              (string-upcase y)))) #t)

(define (lists-same? alox aloy op)
  (cond [(and (empty? alox) (empty? aloy)) #t]
        [(or (empty? alox) (empty? aloy)) #f]
        [else (if (op (first alox) (first aloy))
                  (lists-same? (rest alox) (rest aloy) op)
                  #f)]))
         



