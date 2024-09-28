;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO 1/2: Design the data necessary to represent a book, which can
;           either be physical or electronic. All books have a title
;           and author. Physical books are either paperback or hardcover,
;           and have some number of pages. Electronic (e-books) have a
;           format (pdf, epub, txt) and a source URL.
;
;           We've gotten you started with the design of a PhysicalBook
;           and EFormat :) So your task is an EBook and a Book!


(define-struct physbook [title author paperback? pages])

; A PhysicalBook is a (make-physbook String String Boolean PosInteger)
; Interpretation: a physical book
; - title is the title of the book
; - author is the author of the book
; - paperback? is #true if paperback, #false if hardcover
; - pages is the number of pages in the book

(define
  PHYSBOOK-DUNE
  (make-physbook "Dune" "Frank Herbert" #true 896))

(define
  PHYSBOOK-JUSTICE
  (make-physbook
   "Doing Justice: A Prosecutor's Thoughts on Crime, Punishment, and the Rule of Law"
   "Preet Bharara"
   #false
   368))

(define (physbook-temp pb)
  (... (physbook-title pb) ...
       (physbook-author pb) ...
       (physbook-paperback? pb) ...
       (physbook-pages pb) ...))

; An EFormat is one of:
; - "pdf"
; - "epub"
; - "txt"
; Interpretation: e-book formats

(define EFORMAT-PDF "pdf")
(define EFORMAT-EPUB "epub")
(define EFORMAT-TXT "txt")

(define (eformat-temp ef)
  (...
   (cond
     [(string=? ef EFORMAT-PDF) ...]
     [(string=? ef EFORMAT-EPUB) ...]
     [(string=? ef EFORMAT-TXT) ...])))

(define-struct ebook [title author ef url])

; A EBook is a (make-ebook String String EFormat String)
; Interpretation: a digital book
; - title is the title of the book
; - author is the author of the book
; - ef is the electronic format of the book
; - url is the source url of the book

(define  EBOOK-1 (make-ebook "title 1" "author 1" EFORMAT-PDF "www.book.com"))
(define  EBOOK-2 (make-ebook "title 2" "author 2" EFORMAT-EPUB "www.onlinebook.com"))
(define  EBOOK-3 (make-ebook "titel 3" "author 3" EFORMAT-TXT "www.readbookontheinternet.com"))

(define (ebook-temp eb)
  (... (ebook-title eb) ...
       (ebook-author eb) ...
       (eformat-temp (ebook-ef eb)) ...
       (ebook-url eb) ...))

; A Book is one of:
; - PhysicalBook
; - EBook
; Interpretation: either a physical or an electronic book
(define BOOK-1 PHYSBOOK-DUNE)
(define BOOK-2 EBOOK-1)

#;(define (book-temp bk)
    (cond [(physbook? bk) ... (physbook-temp bk) ...]
          [(ebook? bk) ... (ebook-temp bk) ...]))
       


; TODO 2/2: Now design the function where-to-find that accepts a book
;           and returns where you can find it: physical books are either
;           in the "hardcover section" or "paperback section", whereas
;           electronic books are found at their URL.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Consider the following data definitions...

; A Genre is one of:
; - "comedy"
; - "drama"
; - "action"
; - "education"
; Interpretation: genre for a video

(define GENRE-COMEDY "comedy")
(define GENRE-DRAMA "drama")
(define GENRE-ACTION "action")
(define GENRE-EDUCATION "education")

(define (genre-temp g)
  (...
   (cond
     [(string=? g GENRE-COMEDY) ...]
     [(string=? g GENRE-DRAMA) ...]
     [(string=? g GENRE-ACTION) ...]
     [(string=? g GENRE-EDUCATION) ...])))


(define-struct video [name duration hd? genre next])

; A StreamingQueue is one of:
; - #false
; - (make-video String PosInteger Boolean Genre StreamingQueue)
; Interpretation: either an empty queue (#false) or a video
; with a name, duration in minutes, whether it's available in HD,
; and its genre.

(define QUEUE-EMPTY #false)

(define QUEUE-CRASH
  (make-video "Crash Course Organic Chemistry #5"
              14 #true GENRE-EDUCATION
              QUEUE-EMPTY))

(define QUEUE-OLIVER
  (make-video
   "Prisons & Jails: Last Week Tonight with John Oliver"
   18 #true GENRE-COMEDY
   QUEUE-CRASH))

(define QUEUE-DUEL
  (make-video
   "Duel" 2 #false GENRE-ACTION QUEUE-OLIVER))

(define QUEUE-STORM
  (make-video
   "Tim Minchin's Storm the Animated Movie"
   11 #false GENRE-DRAMA
   QUEUE-DUEL))

(define (sq-temp sq)
  (...
   (cond
     [(boolean? sq) ...]
     [(video? sq)
      (...
       (video-name sq) ...
       (video-duration sq) ...
       (video-hd? sq) ...
       (genre-temp (video-genre sq)) ...
       (sq-temp (video-next sq)) ...)])))


; TODO 1/1: Design the following functions. For clarity, we've provided
;           (commented out) tests for each. Don't forget to follow the
;           templates!!!
;
;           Note: for full credit, you only need to submit your attempt
;           for ONE function; however, we recommend trying them all! :)


; Design the function good-for-friday? that determines if a streaming queue
; contains any content that is comedy or action.


(check-expect (good-for-friday? QUEUE-EMPTY) #false)
(check-expect (good-for-friday? QUEUE-STORM) #true)

; good-for-friday? : StreamingQueue -> Boolean
; determines if a streaming qeue contains any content that is comedy or action

(define (good-for-friday? sq)
  (cond
    [(boolean? sq) #false]
    [(video? sq)
     (if (or (string=? GENRE-COMEDY (video-genre sq))
             (string=? GENRE-ACTION (video-genre sq))) #true
                                                       (good-for-friday? (video-next sq)))]))

    



; Design the function duration that calculates the total number of minutes
; of content in a streaming queue. For example, an empty queue has 0 minutes
; of content, whereas QUEUE-STORM has 45 minutes (14 + 18 + 2 + 11).


(check-expect (duration QUEUE-EMPTY) 0)
(check-expect (duration QUEUE-STORM) 45)


; duration: StreamingQueue -> PosInteger
; finds the total number of minutes of content in a StreamingQueue
(define (duration sq)
  (cond
    [(boolean? sq) 0]
    [(video? sq)
     (+ (video-duration sq)
        (duration (video-next sq)))]))



; Design the function upgrade that takes a streaming queue and produces a
; new queue containing HD versions of all the videos in the original queue.

#|
(check-expect (upgrade QUEUE-EMPTY) QUEUE-EMPTY)

(check-expect
 (upgrade QUEUE-STORM)
 (make-video
  "Tim Minchin's Storm the Animated Movie"
  11 #true GENRE-DRAMA
  (make-video
   "Duel" 2 #true GENRE-ACTION
   QUEUE-OLIVER)))
|#

