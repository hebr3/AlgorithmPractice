#lang racket
(provide Point is_attacked?)

;; hackerearth recursion

;;   1 2 3 4
;; 1   p
;; 2       p   
;; 3 p
;; 4     p
;;
;; Points (1 2) (2 4) (3 1) (4 3)

;; STRUCTURE
;; (-> Int Int (List Int Int))
(struct Point [x y] #:transparent)

;; PREDICATES
;; (-> Point (Listof Point) Boolean)


;; check if a position on the board is under attack
(define (is_attacked? p L)
  ;; checking for row and column 
  (define (check-row? p L)
    (ormap (lambda (i) (= (Point-x p) (Point-x i))) L))
  (define (check-col? p L)
    (ormap (lambda (i) (= (Point-y p) (Point-y i))) L))

  ;; checking for diagonals
  (define (check-dig+? p L)
    (ormap (lambda (i) (= (+ (Point-y p) (Point-x p)) (+ (Point-y i) (Point-x i)))) L))
  (define (check-dig-? p L)
    (ormap (lambda (i) (= (+ (Point-y p) (Point-x i)) (+ (Point-x p) (Point-y i)))) L))

  ;; return TRUE if any are true
  (or (check-row? p L)
      (check-col? p L)
      (check-dig+? p L)
      (check-dig-? p L)))

(eq? #f (is_attacked? (Point 1 1) (list (Point 2 3) (Point 3 5) (Point 4 2) (Point 5 4))))
(eq? #f (is_attacked? (Point 6 1) (list (Point 2 3) (Point 3 5) (Point 4 2) (Point 5 4))))
(eq? #f (is_attacked? (Point 6 6) (list (Point 2 3) (Point 3 5) (Point 4 2) (Point 5 4))))
(eq? #t (is_attacked? (Point 1 8) (list (Point 2 3) (Point 3 5) (Point 4 2) (Point 5 4))))
(eq? #t (is_attacked? (Point 5 7) (list (Point 2 3) (Point 3 5) (Point 4 2) (Point 5 4))))
(eq? #t (is_attacked? (Point 6 7) (list (Point 2 3) (Point 3 5) (Point 4 2) (Point 5 4))))
(eq? #t (is_attacked? (Point 7 5) (list (Point 2 3) (Point 3 5) (Point 4 2) (Point 5 4))))

