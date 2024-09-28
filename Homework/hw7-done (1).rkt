;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hw7-done) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This problem asks you to design several functions that employ the
; following data definitions. The functions that you design *must* use
; list abstraction(s) when appropriate; you MAY NOT use recursion: doing
; so will lead you to get no code credit for the function :(
;
; NOTE #1: Part of the credit for each problem will be based on the choice
; of list abstractions, so make sure that they are a good match for the
; problem.
;
; NOTE #2: For certain problems, you will have to design helper functions
; that do not use list abstractions. You should follow the full design
; recipe (including appropriate use of templates) for all problems. Be sure
; to do this, even if it feels a bit tedious - listen to your templates!!
;
; Data Definitions (do not modify these)


; A Weekday is one of:
; - "Monday"
; - "Tuesday"
; - "Wednesday"
; - "Thursday"
; - "Friday"
; Interpretation: a day that excludes the weekend

(define WEEKDAY-M "Monday")
(define WEEKDAY-T "Tuesday")
(define WEEKDAY-W "Wednesday")
(define WEEKDAY-R "Thursday")
(define WEEKDAY-F "Friday")

(define (weekday-temp w)
  (...
   (cond
     [(string=? w WEEKDAY-M) ...]
     [(string=? w WEEKDAY-T) ...]
     [(string=? w WEEKDAY-W) ...]
     [(string=? w WEEKDAY-R) ...]
     [(string=? w WEEKDAY-F) ...])))


(define-struct meeting [day bname rnum hstart mstart duration])

; A ClassMeeting is a (make-meeting Weekday String String PosInt[8, 18] NonNegInt[0, 59] PosInt)
; Interpretation: when a class is scheduled to meet weekly
; - day: which day of the week
; - bname: name of the building
; - rnum: room number
; - hstart: starting hour (24hr)
; - mstart: starting minute
; - duration: length of the class (in minutes)

(define CM-FUNDIES-M (make-meeting WEEKDAY-M "WVH" "210A" 10 30 65))
(define CM-FUNDIES-W (make-meeting WEEKDAY-W "WVH" "210A" 10 30 65))
(define CM-FUNDIES-R (make-meeting WEEKDAY-R "WVH" "210A" 10 30 65))
;
(define CM-FUNDIES-LAB (make-meeting WEEKDAY-T "WVH" "212" 8 0 100))
;
(define CM-DISCRETE-T (make-meeting WEEKDAY-T "ISEC" "102" 13 35 100))
(define CM-DISCRETE-F (make-meeting WEEKDAY-F "ISEC" "102" 13 35 100))
;
(define CM-DISCRETE-SEM (make-meeting WEEKDAY-W "Hastings" "110" 16 35 65))
;
(define CM-CREATURES-T (make-meeting WEEKDAY-T "Forbidden Forest" "Hut" 13 0 200))
(define CM-POTIONS-R (make-meeting WEEKDAY-R "Hogwarts" "Dungeon" 13 0 200))

(define (classmeeting-temp cm)
  (... (weekday-temp (meeting-day cm)) ...
       (meeting-bname cm) ...
       (meeting-rnum cm) ...
       (meeting-hstart cm) ...
       (meeting-mstart cm) ...
       (meeting-duration cm) ...))


(define-struct course [prefix num name prof meetings])

; A Course is a (make-course String String String String [List-of ClassMeeting])
; Interpretation: a weekly class
; - prefix: the course prefix
; - num: the course number
; - name: the course name
; - prof: name of the professor
; - meetings: weekly meeting times

(define COURSE-EASY-A
  (make-course "SCHED" "101" "Easy A" "Lazy"
               '()))

(define COURSE-FUNDIES-LECTURE
  (make-course "CS" "2500" "Fundies" "Howdy"
               (list CM-FUNDIES-M CM-FUNDIES-W CM-FUNDIES-R)))

(define COURSE-FUNDIES-LAB
  (make-course "CS" "2501" "Fundies Lab" "Awesome TAs"
               (list CM-FUNDIES-LAB)))

(define COURSE-DISCRETE-LECTURE
  (make-course "CS" "1800" "Discrete Structures" "Dr Strange"
               (list CM-DISCRETE-T CM-DISCRETE-F)))

(define COURSE-DISCRETE-SEM
  (make-course "CS" "1802" "Seminar for CS 1800" "Park"
               (list CM-DISCRETE-SEM)))

(define COURSE-CREATURES
  (make-course "HPTR" "2000" "Care of Magical Creatures" "Hagrid"
               (list CM-CREATURES-T)))

(define COURSE-POTIONS
  (make-course "HPTR" "2650" "Potions" "Snape"
               (list CM-POTIONS-R)))

(define (course-temp c)
  (... (course-prefix c) ...
       (course-num c) ...
       (course-name c) ...
       (course-prof c) ...
       (locm-temp (course-meetings c)) ...))


; A CourseSchedule is a [List-of Course]
; Interpretation: a list of weekly courses!

(define SCHEDULE-OOPS '())

(define SCHEDULE-KHOURY
  (list COURSE-FUNDIES-LECTURE
        COURSE-FUNDIES-LAB
        COURSE-DISCRETE-LECTURE
        COURSE-DISCRETE-SEM))

(define SCHEDULE-MAGIC
  (list COURSE-CREATURES
        COURSE-POTIONS))

(define SCHEDULE-CS+MAGIC
  (list COURSE-FUNDIES-LECTURE
        COURSE-FUNDIES-LAB
        COURSE-CREATURES
        COURSE-POTIONS))


; TODO 1/8: Part of healthy course scheduling is making sure to build in time for
;           food, and so you are to design the function lunch-course that produces
;           a "Lunch" course!
;
;           The function should take in a prefix & number (e.g., "FOOD" "101"),
;           a name & professor (e.g., "Exciting Baking" with "Alderton"), as
;           well as a list of weekdays. The function will then makes sure that
;           déjeuner occurs on all of those days at noon (for one hour) in a
;           single location of your choice (e.g., Hogwarts Great Hall).
;
;           Note: make sure to test your function on at least two sets of inputs!

(define LUNCH-M (make-meeting WEEKDAY-M "Wendy's" "1" 12 0 60))
(define LUNCH-T (make-meeting WEEKDAY-T "Wendy's" "1" 12 0 60))
(define LUNCH-W (make-meeting WEEKDAY-W "Wendy's" "1" 12 0 60))
(define LUNCH-R (make-meeting WEEKDAY-R "Wendy's" "1" 12 0 60))
(define LUNCH-F (make-meeting WEEKDAY-F "Wendy's" "1" 12 0 60))

; lunch-course : String String String String [List-of Weekday] -> Course
; makes a lunch course with the given parameters that meets every day at noon
(check-expect (lunch-course "CS" "1800" "Discrete Structures" "Dr Strange"
                            (list WEEKDAY-M WEEKDAY-T))
              (make-course "CS" "1800" "Discrete Structures" "Dr Strange"
                           (list (make-meeting WEEKDAY-M "Wendy's" "1" 12 0 60)
                                 (make-meeting WEEKDAY-T "Wendy's" "1" 12 0 60))))
(check-expect (lunch-course "celtics" "will" "win the 2022-23" "nba title."
                            (list WEEKDAY-T WEEKDAY-W WEEKDAY-R WEEKDAY-F))
              (make-course "celtics" "will" "win the 2022-23" "nba title."
                           (list (make-meeting WEEKDAY-T "Wendy's" "1" 12 0 60)
                                 (make-meeting WEEKDAY-W "Wendy's" "1" 12 0 60)
                                 (make-meeting WEEKDAY-R "Wendy's" "1" 12 0 60)
                                 (make-meeting WEEKDAY-F "Wendy's" "1" 12 0 60))))
(check-expect (lunch-course "GIVE" "ME" "A 100 ON THIS ASSIGNMENT" "PLEASE!!!!!!"
                            '())
              (make-course "GIVE" "ME" "A 100 ON THIS ASSIGNMENT" "PLEASE!!!!!!"
                           '()))

(define (lunch-course prefix num name prof alow)
  (make-course prefix num name prof (map every-day alow)))

; every-day : Weekday -> Meeting
; makes an hour long meeting at 12pm exactly at Wendy's room 1 on a given weekday
(check-expect (every-day WEEKDAY-T) (make-meeting WEEKDAY-T "Wendy's" "1" 12 0 60))
(check-expect (every-day WEEKDAY-W) (make-meeting WEEKDAY-W "Wendy's" "1" 12 0 60))
(check-expect (every-day WEEKDAY-F) (make-meeting WEEKDAY-F "Wendy's" "1" 12 0 60))

(define (every-day weekday) 
  (make-meeting weekday "Wendy's" "1" 12 0 60))


; TODO 2/8: Design the function long-weekend? that determines if a
;           course schedule avoids all classes on Mondays & Fridays.
;           In the examples above, this is true of OOPS and MAGIC.
;           Note: make sure to follow all the templates and
;           sufficiently test all your functions!

;; long-weekend? CourseSchedule -> Boolean
;; determines if a course schedule avoids all classes on Mondays & Fridays
(check-expect (long-weekend? SCHEDULE-OOPS) #t)
(check-expect (long-weekend? SCHEDULE-KHOURY) #f)
(check-expect (long-weekend? SCHEDULE-MAGIC) #t)
(check-expect (long-weekend? SCHEDULE-CS+MAGIC) #f)

(define (long-weekend? schedule)
  (andmap avoidsmf? schedule))

;; avoidsmf? Course -> Boolean
;; returns true if a course does not occur on mondays and fridays, returns false otherwise
(check-expect (avoidsmf? COURSE-EASY-A) #t)
(check-expect (avoidsmf? COURSE-FUNDIES-LECTURE) #f)
(check-expect (avoidsmf? COURSE-FUNDIES-LAB) #t)
(check-expect (avoidsmf? COURSE-DISCRETE-LECTURE) #f)
(check-expect (avoidsmf? COURSE-DISCRETE-SEM) #t)

(define (avoidsmf? course)
  (andmap hasmf? (course-meetings course)))

;; hasmf? ClassMeeting -> Boolean
;; returns true if a particular class meeting does not meet on monday or friday
(check-expect (hasmf? CM-FUNDIES-M) #f)
(check-expect (hasmf? CM-FUNDIES-W) #t)
(check-expect (hasmf? CM-FUNDIES-R) #t)
(check-expect (hasmf? CM-DISCRETE-F) #f)

(define (hasmf? meeting) 
  (cond [(string=? (meeting-day meeting) WEEKDAY-M) #f]
        [(string=? (meeting-day meeting) WEEKDAY-F) #f]
        [else #t]))
  

; TODO 3/8: Design the function only-khoury that takes a course schedule
;           and produces a new schedule only containing classes that
;           have the prefix "CS", "DS", or "CY". So supplying OOPS and
;           KHOURY would result in unaffected schedules, but MAGIC would
;           result in an empty schedule and CS+MAGIC would result in a
;           schedule with only Fundies :)
;
;           Note: since we didn't include any DS/CY courses in the
;           examples, make may need to create example courses to properly
;           test your helper function(s)! Some course suggestions include
;           DS2000 (Programming with Data) and CY2550 (Foundations of
;           Cybersecurity).

(define COURSE-DS
  (make-course "DS" "2000" "Programming with Data" "Smith"
               (list CM-FUNDIES-LAB)))

(define COURSE-CY
  (make-course "CY" "2550" "Foundations of Cybersecurity" "Yoda"
               (list CM-DISCRETE-F CM-CREATURES-T)))

(define SCHEDULE-1 (list COURSE-FUNDIES-LECTURE
                         COURSE-CREATURES
                         COURSE-CY
                         COURSE-DS))

(define SCHEDULE-2 (list COURSE-DS
                         COURSE-POTIONS
                         COURSE-CY))

; only-khoury : CourseSchedule -> CourseSchedule
; creates a new course schedule removing all non-khoury classes from the given schedule
(check-expect (only-khoury SCHEDULE-OOPS) SCHEDULE-OOPS)
(check-expect (only-khoury SCHEDULE-KHOURY) SCHEDULE-KHOURY)
(check-expect (only-khoury SCHEDULE-MAGIC) '())
(check-expect (only-khoury SCHEDULE-CS+MAGIC) (list COURSE-FUNDIES-LECTURE
                                                    COURSE-FUNDIES-LAB))
(check-expect (only-khoury SCHEDULE-1) (list COURSE-FUNDIES-LECTURE
                                             COURSE-CY
                                             COURSE-DS))
(check-expect (only-khoury SCHEDULE-2) (list COURSE-DS
                                             COURSE-CY))

(define (only-khoury schedule)
  (filter khoury? schedule))



; khoury? : Course -> Boolean
; returns true if a given course is within khoury, i.e. has the prefix CS, DS or CY
(check-expect (khoury? COURSE-FUNDIES-LECTURE) #t)
(check-expect (khoury? COURSE-POTIONS) #f)
(check-expect (khoury? COURSE-DS) #t)
(check-expect (khoury? COURSE-CY) #t)
(check-expect (khoury? COURSE-CREATURES) #f)

(define (khoury? course)
  (or (string=? (course-prefix course) "CS")
      (string=? (course-prefix course) "DS")
      (string=? (course-prefix course) "CY")))

; TODO 4/8: Design the function time-in-class that calculates total
;           time spent in class (in minutes each week) for a supplied
;           course schedule. For example, OOPS requires 0 minutes and
;           KHOURY is 560.

; time-in-class : CourseSchedule -> Real
; calculates total number of minutes spent in class each week for a given schedule
(check-expect (time-in-class SCHEDULE-OOPS) 0)
(check-expect (time-in-class SCHEDULE-KHOURY) 560)
(check-expect (time-in-class SCHEDULE-MAGIC) 400)
(check-expect (time-in-class SCHEDULE-CS+MAGIC) 695)

(define (time-in-class schedule)
  (foldr course-minutes 0 schedule))

; course-minutes : Course Real -> Real
; calculates total number of minutes spent in class each week for a given course
(check-expect (course-minutes COURSE-FUNDIES-LAB 0) 100)
(check-expect (course-minutes COURSE-FUNDIES-LECTURE 0) 195)
(check-expect (course-minutes COURSE-POTIONS 150) 350)
(check-expect (course-minutes COURSE-DISCRETE-LECTURE -200) 0)

(define (course-minutes course n)
  (foldr meeting-minutes n (course-meetings course)))

; meeting-minutes Meeting Real -> Real
; adds number of minutes spent in a given meeting to a given real number n
(check-expect (meeting-minutes CM-FUNDIES-M 0) 65)
(check-expect (meeting-minutes CM-DISCRETE-F 50) 150)
(check-expect (meeting-minutes CM-POTIONS-R 100) 300)

(define (meeting-minutes meeting n)
  (+ n (meeting-duration meeting)))


; TODO 5/8: Design the function bring-water? that takes a course schedule
;           and determines if any course has even a single meeting that
;           lasts for longer than two hours. For example, this is true
;           for either of the magic schedules, but none of the others.

;; bring-water? CourseSchedule -> Boolean
;; determines if any course in a schedule has even a single meeting that
;; lasts for longer than two hours
(check-expect (bring-water? SCHEDULE-OOPS) #f)
(check-expect (bring-water? SCHEDULE-KHOURY) #f)
(check-expect (bring-water? SCHEDULE-MAGIC) #t)
(check-expect (bring-water? SCHEDULE-CS+MAGIC) #t)

(define (bring-water? schedule)
  (ormap contains-a-long-meeting? schedule))

;; contains-a-long-meeting? : Course -> Boolean
;; determines if a course has at least one meeting longer than two hours
(check-expect (contains-a-long-meeting? COURSE-EASY-A) #f)
(check-expect (contains-a-long-meeting? COURSE-FUNDIES-LECTURE) #f)
(check-expect (contains-a-long-meeting? COURSE-FUNDIES-LAB) #f)
(check-expect (contains-a-long-meeting? COURSE-DISCRETE-LECTURE) #f)
(check-expect (contains-a-long-meeting? COURSE-DISCRETE-SEM) #f)
(check-expect (contains-a-long-meeting? COURSE-CREATURES) #t)
(check-expect (contains-a-long-meeting? COURSE-POTIONS) #t)

(define (contains-a-long-meeting? c)
  (ormap is-long-meeting? (course-meetings c)))

;; is-long-meeting: ClassMeeting -> Boolean
;; determines if a meeting lasts longer than two hours
(check-expect (is-long-meeting? CM-FUNDIES-M) #f)
(check-expect (is-long-meeting? CM-FUNDIES-LAB) #f)
(check-expect (is-long-meeting? CM-DISCRETE-F) #f)
(check-expect (is-long-meeting? CM-DISCRETE-SEM) #f)
(check-expect (is-long-meeting? CM-CREATURES-T) #t)
(check-expect (is-long-meeting? CM-POTIONS-R) #t)

(define (is-long-meeting? m)
  (> (meeting-duration m) 120))
         

; TODO 6/8: Design the function course->days-abbrev that takes a course
;           and produces a single string that has abbreviations of all
;           days of the week that course meets. For instance, Fundies
;           lecture would produce "MWR", Fundies lab would produce "T",
;           Discrete lecture would be "TF", and the "easy A" class would
;           produce "" (since the lazy prof never wants to meet!).

; course->days-abbrev : Course -> String
; takes a course and produces a single string that has
; abbreviations of all days of the week that course meets
(check-expect (course->days-abbrev COURSE-FUNDIES-LECTURE) "MWR")
(check-expect (course->days-abbrev COURSE-FUNDIES-LAB) "T")
(check-expect (course->days-abbrev COURSE-DISCRETE-LECTURE) "TF")
(check-expect (course->days-abbrev COURSE-EASY-A) "")

(define (course->days-abbrev course)
  (foldl days? "" (course-meetings course)))

; days? : Meeting String -> String
; appends a given string to an abbreviation of the day of a given meeting
(check-expect (days? CM-FUNDIES-M "MW") "MWM")
(check-expect (days? CM-DISCRETE-F "MTWR") "MTWRF")
(check-expect (days? CM-POTIONS-R "bruh") "bruhR")
(check-expect (days? CM-DISCRETE-SEM "") "W")
(check-expect (days? CM-FUNDIES-M "yomammasougly") "yomammasouglyM")

(define (days? meeting str)
  (string-append str (if (or (string=? "Monday" (meeting-day meeting))
                             (string=? "Tuesday" (meeting-day meeting))
                             (string=? "Wednesday" (meeting-day meeting))
                             (string=? "Friday" (meeting-day meeting)))
                         (substring (meeting-day meeting) 0 1)
                         "R")))


; TODO 7/8: Design the functions stack/h and stack/v, to stack a supplied
;           list of images horizontally and vertically, with a bit of buffer
;           between each image (see the GAP we've defined for you). You have
;           been supplied tests for clarity.

(define GAP (square 5 "solid" "white"))

; stack/h: [List-of Image] -> Image
; stacks a supplied list of images horizontally
(check-expect
 (stack/h '())
 GAP)
(check-expect
 (stack/h
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (beside
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/h loi)
  (foldr beside GAP (map beside-gap loi)))

; beside-gap: Image -> Image
; places a gap beside a given image
(check-expect (beside-gap (text "A" 5 "black")) (beside GAP (text "A" 5 "black")))
(check-expect (beside-gap (square 40 "solid" "blue")) (beside GAP (square 40 "solid" "blue")))

(define (beside-gap image)
  (beside GAP image))


; stack/v: [List-of Image] -> Image
; stacks a supplied list of images vertically
(check-expect
 (stack/v '())
 GAP)

(check-expect
 (stack/v
  (list
   (text "A" 5 "black")
   (text "B" 10 "black")
   (text "C" 50 "black")))
 (above
  GAP
  (text "A" 5 "black")
  GAP
  (text "B" 10 "black")
  GAP
  (text "C" 50 "black")
  GAP))

(define (stack/v loi)
  (foldr above GAP (map above-gap loi)))

; above-gap: Image -> Image
; places a gap above a given image
(check-expect (above-gap (text "A" 5 "black")) (above GAP (text "A" 5 "black")))
(check-expect (above-gap (square 40 "solid" "blue")) (above GAP (square 40 "solid" "blue")))

(define (above-gap image)
  (above GAP image))

; TODO 8/8: Now using your solutions to the previous two parts, design the
;           function viz-schedule, which produces a visual representation
;           of a supplied course schedule, such that each course is a row
;           (with the prefix, num, name, prof, and day abbreviations) and
;           the rows are vertically stacked. You have been supplied tests
;           for clarity.

; viz-schedule: CourseSchedule -> Image
; produces a visual representation of a supplied course schedule
(check-expect (viz-schedule SCHEDULE-OOPS) (overlay/align "middle" "middle"
                                                          GAP
                                                          (empty-scene 900 600)))
(check-expect (viz-schedule SCHEDULE-KHOURY)
              (overlay/align "middle" "middle"
                             (above GAP
                                    (text "CS 2500 (Fundies, Howdy): MWR" 30 "black")
                                    GAP
                                    (text "CS 2501 (Fundies Lab, Awesome TAs): T" 30 "black")
                                    GAP
                                    (text "CS 1800 (Discrete Structures, Dr Strange): TF" 30 "black")
                                    GAP
                                    (text "CS 1802 (Seminar for CS 1800, Park): W" 30 "black")
                                    GAP)
                             (empty-scene 900 600)))

(define (viz-schedule schedule)
  (overlay/align "middle" "middle" (stack/v (map viz-course schedule)) (empty-scene 900 600)))

; viz-course: Course -> Image
; produces a visual representation of a supplied course
(check-expect (viz-course COURSE-FUNDIES-LECTURE) (text "CS 2500 (Fundies, Howdy): MWR" 30 "black"))
(check-expect (viz-course COURSE-EASY-A) (text "SCHED 101 (Easy A, Lazy): " 30 "black"))

(define (viz-course course)
  (text (string-append (course-prefix course) " "
                       (course-num course) " ("
                       (course-name course) ", "
                       (course-prof course) "): "
                       (course->days-abbrev course))
        30 "black"))

