#lang sicp

; 1.1
; ok I did this one in my head

; 1.2
(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
   (* 3 (- 6 2) (- 2 7))
)

; 1.3
(define (sum-big-squares x y z)
  (cond ((and (> x y) (> z y)) (+ (* x x) (* z z)))
       ((and (> y x) (> z x)) (+ (* y y) (* z z)))
       (else (+ (* x x) (* y y)))
   )
)

; 1.3 Tests
(= (sum-big-squares 1 2 3) 13)
(= (sum-big-squares 3 2 1) 13)
(= (sum-big-squares 2 3 1) 13)
(= (sum-big-squares 5 5 5) 50)
(= (sum-big-squares 7 7 3) 98)
(= (sum-big-squares 5 3 9) 106)

; 1.4
; The expression is evaluated with either + or - depending
; on the result of the (> b 0) comparison.

; 1.5
; Applicative order goes into an infinite loop because the interpreter
; tries to evaluate (p) before running the (test) function.
; Normal order will return 0 because it does not need to evaluate (p)
; to resolve the if statement.

; 1.6
; The (sqrt-iter) function will call itself recursively forever
; because it will evaluate both sides of the (cond) statement.
; A normal (if) clause will only evaluate the righthand side if
; the lefthand side is false.

; 1.7
; Procedures from the book
(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x) (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))

(sqrt .16) ; 0.40091152852260414
(sqrt 16) ; 4.000000636692939
(sqrt 161616161616) ; 402015.12610348384

; The "good enough" check counts something like 4.000000001 as
; close enough to 4, even though the square root of 16 is the integer 4.
; The issue with larger numbers is analogous.

; New procedure
(define (good-enough-new? guess prev-guess)
  (< (abs (/ (- prev-guess guess) guess)) 0.000000001))
  
(define (sqrt-iter-new guess x) (if (good-enough-new? guess (improve guess x))
  guess
  (sqrt-iter-new (improve guess x) x)))

(define (sqrt-new x) (sqrt-iter-new 1.0 x))

(sqrt-new 16) ; 4.000000000000051
(sqrt-new .16) ; 0.4000000000013423
(sqrt 161616161616) ; 402015.12610348384

; The answers are marginally more precise than the previous version 🤔

; 1.8
(define (cbrt-improve guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))

(define (cbrt-iter guess x)
  (if (good-enough-new? guess (cbrt-improve guess x))
  guess
  (cbrt-iter (cbrt-improve guess x) x)))

(define (cbrt x) (cbrt-iter 1.0 x))

(cbrt 9) ; ~2.08
(cbrt 27) ; ~3

; 1.9

; First procedure:
; (inc (inc (inc (+ 1 5)))) and so on. I'm not writing it all out!

; Second procedure:
; (+ 4 5)
; (+ 3 6)
; and so on until (+ 0 9)

; The first procedure is recursive because it 