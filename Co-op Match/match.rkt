;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname match) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "a08helpers.rkt")
(require "ranking.rkt")

;; (sum emp-id std-id employers students) consumes an EmpID, StdID,
;;   (listof EmpRanking) and (listof StdRanking), and produces a FlatRanking, 
;;      with the preference as the sum of student and employer preferences.
;; sum: EmpID StdID (listof EmpRanking) (listof StdRanking) -> FlatRanking

(define (sum emp-id std-id employers students)
  (list emp-id std-id
        (+ (find-pref employers emp-id std-id)
           (find-pref students std-id emp-id)
           (random-epsilon emp-id std-id))))

;; (potential-matches employers students) consumes a
;;   (listof EmpRanking) and a (listof StdRanking), and produces a list of all
;;     potential matches.
;; potential-matches: (listof EmpRanking) (listof StdRanking)
;;                     -> (listof FlatRanking)

(define (potential-matches employers students)
  (map (lambda (x) (sum (first x) (second x) employers students))
       (unfold employers)))

;; (create-matches lofr) consumes a list of potential matches as a
;;   (listof FlatRanking), and produces a list of matches as a
;;     (list (list Str Str)).
;; create-matches: (listof FlatRanking) -> (listof (list EmpId StdId))      

(define (create-matches lofr)
  (cond [(empty? lofr)
         empty]
        [else
         (cons (list (first (argmin third lofr))
                     (second (argmin third lofr)))
               (create-matches
                (expunge lofr
                         (first (argmin third lofr))
                         (second (argmin third lofr)))))]))

;; (match employers students) consumes a (listof EmpRanking) and
;;   (listof StdRanking), and produces a list of matches as a
;;     (listof (list EmpId StdId))
;; match: (listof EmpRanking) (listof StdRanking) -> (listof (list EmpId StdId))
;; requires: every student ranked in employers is present in students and
;;           ranks that employer

(define (match employers students)
  (create-matches (potential-matches employers students)))
