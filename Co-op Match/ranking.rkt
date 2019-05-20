;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ranking) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "provide.rkt")

(provide expunge)
(provide ranking-sort)
(provide unfold)
(provide find)
(provide find-pref)

;; An employer ID, EmpId, is a Str (e.g. "Google")
;; A student ID, StdId, is a Str (e.g. "Anna")
;; An Id is (anyof EmpId StdId)
;; A preference, Pref, is a Nat (e.g. 2)
;; requires: a preference is >= 1

;; An EmpRanking is (list EmpId (listof (list StdId Pref)))
;; requires: The list of preferences is in non-decreasing order
;; and non-empty.

;; A StdRanking is (list StdId (listof (list EmpId Pref)))
;; requires: The list of preferences is in non-decreasing order
;; and non-empty.

;; A Ranking is (anyof EmpRanking StdRanking)

;; (expunge lofr ranking-id ranked-id) consumes a (listof FlatRanking), a 
;;   ranking ID and and a ranked ID. It produces the same list of flat rankings  
;;     but without elements containing ranking-id or ranked-id.
;; expunge: (listof FlatRanking) ID ID -> (listof FlatRanking)

(define (expunge lofr ranking-id ranked-id)
  (filter (lambda (fr) (and (not (string=? ranking-id (first fr)))
                            (not (string=? ranked-id (second fr))))) lofr))

;; (ranking-sort lofr) consumes a (listof FlatRanking) and produces the data  
;;   reordered so that the ranking IDs are in increasing alphabetical order. 
;;     Where two ranking IDs are the same, they are ordered by the ranked IDs.
;; ranking-sort: (listof FlatRanking) -> (listof FlatRanking)

(define (ranking-sort lofr)
  (quicksort lofr (lambda (x rr) (string<?
                                  (string-append (first x) (second x))
                                  (string-append (first rr) (second rr))))))

;; (unfold lor) consumes a (listof Ranking) and produces an equivalent
;;   (listof FlatRanking).
;; unfold: (listof Ranking) -> (listof FlatRanking)

(define (unfold lor)
  (foldr (lambda (a b) (append a b))
         empty
         (map (lambda (x)
                (map (lambda (y) (cons (first x) y))
                     (second x)))
              lor)))

;; (find key lolst) consumes a key and a list of lists. It produces the first  
;;   element within the list that has a first element equal to the key, or false 
;;      if there is no such element.
;; find-key: Any (listof (listof Any)) -> (listof (listof Any))

(define (find key lolst)
  (foldr
   (lambda (x y)
     (cond [(equal? (foldr (lambda (a b) (cond [(equal? key a) a] [else b]))
                           (list key) ;; ensures the base case =/= key
                           x) key) x]
           [else y]))
   #false lolst))

;; (find-pref loranking ranking-id ranked-id) consumes a (listof Ranking),
;;    a ranking ID, and a ranked ID. It produces the preference of the ranking 
;;     ID for the ranked ID. 
;; find-pref: (listof Ranking) ID ID -> Preference
;; requires: preference must exist within the list of rankings.

(define (find-pref loranking ranking-id ranked-id)
  (second
   (first
    (filter (lambda (y) (string=? (first y) ranked-id))
            (second
             (first
              (filter (lambda (x)
                        (string=? (first x) ranking-id)) loranking)))))))