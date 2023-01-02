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
(sqrt-new 161616161616) ; 402015.12610348384

; The answers are marginally more precise than the previous version 🤔
; 161616161616 gets an identical result though. It seems to have
; only improved for small numbers.

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

; The first procedure is recursive because it increments the result of its
; recursive call. The second procedure is iterative because it simply
; returns the result of calling itself, allowing the Scheme interpreter
; to optimize the procedure into an iterative process.

; 1.10

; (I assume I'm meant to calculate these by hand instead of
; just running them in the interpreter.)

; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; ...
; (A 0 (A 0 (A 0... A 1 1)))
; The (* 2 y) when y = 2 cascades upward nine times, resulting in 1024

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2))
; (A 1 (A 1 (A 0 (A 1 1)))
; ...
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 3))))
; (A 1 (A 0 (A 0 (A 1 (A 1 2))))
; ...
; (A 1 16)
; Based on the method above, the solution is 2^16 = 65536

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; Hey this looks familiar
; ...
; 65536

; 1.11

; Recursive
(define (eleven-rec n)
  (if (< n 3)
      n
      (+ (eleven-rec (- n 1))
         (* 2 (eleven-rec (- n 2)))
         (* 3 (eleven-rec (- n 3))))))

; Iterative
(define (eleven-iter n a b c new-n)
  (if (= n new-n)
      a
      (eleven-iter n (+ a (* 2 b) (* 3 c)) a b (+ 1 new-n))))

(define (eleven n)
  (if (< n 3)
      n
      (eleven-iter n 2 1 0 2)))

(= (eleven-rec 5) 25)
(= (eleven 5) 25)

; 1.12
(define (pascal row-number col-number)
  (if (or (= col-number 1) (= col-number row-number))
      1
      (+ (pascal (- row-number 1) (- col-number 1))
         (pascal (- row-number 1) col-number))))

(= (pascal 10 7) 84)
(= (pascal 1 1) 1)

; 1.13
; What?

; 1.14
; I don't know what "the tree" is - maybe a special notation
; that would have been explained in the accompanying class?

; The process loops x - 1 times for each kind of coin added,
; where x is the kinds of coins. This makes it O^n in modern
; O notation, no idea in the weird notation this book uses.

; Given that the procedure's `cond` statement covers 1 through 5,
; the worst-case scenario is O(n^5) which is pretty slow.

; 1.15
; a.
; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.05))))))

; 6 times

; b.
; The number of times `a` must be divided by 3
; to hit < 0.1, so uhhhhhh `(a/3n) < 0.1`.

; 1.16

(define (iter-expt b n)
  (define (compute-exp b1 n1 a)
    (cond ((= n1 0) a)
          ((even? n1) (compute-exp (square b1) (/ n1 2) a))
          (else (compute-exp b1 (- n1 1) (* a b1)))))
  (compute-exp b n 1))

(= (iter-expt 5 6) 15625)
(= (iter-expt 5 12) 244140625)
(= (iter-expt 11 6) 1771561)

