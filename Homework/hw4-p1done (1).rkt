;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw4-p1done) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Let's think about what goes into designing notifications for a mobile device.
;
; Consider the following data definitions...


;(define-struct info [app message]) 

; An InfoMessage is a (make-info String String)
; Interpretation: a message from an app

;(define-struct badge [app num])

; A Badge is a (make-badge String Nat)
; Interpretation: a numeric indicator for an app

;(define-struct confirm [app yestxt notxt])

; A Confirmation is a (make-confirm String String String)
; Interpretation: a yes/no question from an app, with
; associated text to display for each option


; TODO 1/2: Complete the design recipe for InfoMessage,
;           Badge, and Confirmation. You should come up
;           with reasonable examples, but are welcome
;           to be creative :)

(define-struct info [app message])

; An InfoMessage is a (make-info String String)
; Interpretation: a message from an app
; - app is the app sending the info message
; - message is the message being sent

(define INFO-0 (make-info "Instagram" "Hello"))
(define INFO-1 (make-info "Snapchat" "What's up"))
(define INFO-2 (make-info "BeReal" "Post BeReal!"))

(define (info-temp info)
  (...
   (info-app info) ...
   (info-message info) ...))

(define-struct badge [app num])

; A Badge is a (make-badge String Nat)
; Interpretation: a numeric indicator for an app
; - app is the app sending the badge
; - num is the number on the badge

(define BADGE-0 (make-badge "Amazon" 5))
(define BADGE-1 (make-badge "Maps" 67))
(define BADGE-2 (make-badge "Gmail" 782))

(define (badge-temp badge)
  (...
   (badge-app badge) ...
   (badge-num badge) ...))

(define-struct confirm [app yestxt notxt])

; A Confirmation is a (make-confirm String String String)
; Interpretation: a yes/no question from an app, with
; associated text to display for each option
; - app is the app asking for confirmation
; - yestxt is the yes option
; - notxt is the no option

(define CONF-0 (make-confirm "Duo" "Yes" "No"))
(define CONF-1 (make-confirm "IMessage" "Yes" "No"))
(define CONF-2 (make-confirm "Zoom" "Yes" "No"))

(define (confirm-temp confirm)
  (...
   (confirm-app confirm) ...
   (confirm-yestxt confirm) ...
   (confirm-notxt confirm) ...))


; TODO 2/2: Design the data type Notification, which represents
;           a single notification that could be of any of the
;           types described above.

; A Notification is one of:
; - Confirmation
; - Badge
; - Info
; Interpretation: a type of smartphone notification

(define NOTIF-1 INFO-0)
(define NOTIF-2 BADGE-2)
(define NOTIF-3 CONF-1)

(define (notif-temp notif)
  (...
   (cond [(info? notif)
          (... (info-app info) ...
               (info-message info) ...)]
         [(badge? notif)
          (... (badge-app badge) ...
               (badge-num badge) ...)]
         [(confirmation? notif)
          (... (confirm-app confirm) ...
               (confirm-yestxt confirm) ...
               (confirm-notxt confirm) ...)])))




