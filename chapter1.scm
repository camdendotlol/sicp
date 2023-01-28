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

; 1.17

(define (double x) (* x 2))
(define (halve x) (/ x 2))

(define (new-multi x y)
  (cond ((= x 1) y)
        ((even? x) (new-multi (halve x) (double y)))
        (else (+ y (new-multi (- x 1) y)))))

(= (new-multi 4 5) 20)
(= (new-multi 3 5) 15)
(= (new-multi 28374 78723) 2233686402)

; 1.18

(define (iter-multi x y)
  (define (compute-multi x1 y1 total)
    (cond ((= x1 0) total)
          ((even? x1) (compute-multi (halve x1) (double y1) total))
          (else (compute-multi (halve (- x1 1)) (double y1) (+ total y1)))))
  (compute-multi x y 0))

(= (iter-multi 10 30) 300)
(= (iter-multi 3 5) 15)
(= (iter-multi 37 20) 740)

; 1.19

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q)) ;🤨
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(= (fib 10) 55)
(= (fib 20) 6765)

; 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Applicative order

; where G = gcd
; (G 206 40)
; (G 40 6)
; (G 6 4)
; (G 4 2)
; (G 2 0)
; 2
; remainder called 4 times

; Normal order

; (G 206 40)
; (G ~(206 (G 40 6))) remainder called!
; (G ~(206 (G ~(40 (G ...)
; basically it calls remainder a lot of times because
; it needs to expand every possibility.
; Sorry Abelson et al., I'm not gonna write it all out!
; Applicative order > normal order, I get it!

; 1.21
(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

; I assume I'm meant to write them out?

; (smallest-divisor 19999)
; (find-divisor 19999 2)
; (find-divisor 19999 3)
; (find-divisor 19999 4)
; ...
; 7

; (smallest-divisor 1999)
; (find-divisor 1999 2)
; ...
; this number is prime so... 1999

; (smallest-divisor 199)
; same for this one. 199.

; 1.22

(runtime) ; wow!

(define (fast-expt b n )
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; APH's solution:
; (define (expmod base exp m)
;   (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 0)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; only accepts odd numbers
; I could add handling for even numbers but the book
; said "checks [...] odd numbers" so I won't bother
(define (search-for-primes first last)
  (if (even? first)
      (search-for-primes (+ first 1) last)
  (cond ((>= first last)
      (display "\nmission accomplished!"))
      (else (timed-prime-test first)
            (search-for-primes (+ first 2) last)))))

; three smallest primes larger than... (including typical compute times)
; (I think ms = microseconds)
; 1000: 1009, 1013, 1019 (1-2ms)
; 10,000: 10007, 10009, 10037 (4-5ms)
; 100,000: 100003, 100019, 100043 (23-38ms)
; 1,000,000: 1000003, 1000033, 1000037 (31-32ms)

; (sqrt 10 is ~3.16)
; The difference between 1000 and 10,000 follows that approximately.
; At higher levels, the jump isn't quite as much as O(sqrt(10).
; I wonder if there's a modern CPU optimization that didn't exist
; when the book came out, or if there's a reporting accuracy issue
; in DrRacket's interpreter.


; 1.23

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (new-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (new-find-divisor n (next test-divisor)))))

; This seems to take 2x longer instead of 0.5x as the book says?
; ...
; Actually, running it repeatedly with and without `next`, it
; varies a lot. Both procedures seem to run slower the first few times
; and then speed up a lot. After they've been sped up, the `next`
; version does seem to be a little under twice as fast, but not
; when comparing their first few runs. I think there is a caching
; thing going on in the CPU that saves time for repetitive tasks.

; 1.24

; Woah! All calculation times dropped to the 0-1 range.
; This even includes gigantic numbers like 1000000000000000000000000.
; Unfortunately this makes it difficult to check the
; exact calculation the book is asking for. The numbers are too low
; for me to see a difference between number sizes. But it is certainly
; a lot faster.

; 1.25

; As with the previous excercise, the runtime numbers are below 0
; most of the time so I can't see any slowdown (as I expect I'm
; supposed to see).

; Examining the code, the only difference I see is that fast-expt
; will compute a large number at the beginning. Normal, sane
; languages of today don't have a problem with that but on the
; 80s clunkers Abelson et al. must have been using, it would have
; been a big deal.

; However, to avoid so-called "big ints", it would be best to
; avoid APH's solution.

; 1.26

; The result of `(expmod base (/ exp 2) m)` will be calculated once
; and send to the square procedure. In LR's code, he runs that
; calculation twice in order to multiply it against itself.

; 1.27

; I have no idea what "congruent modulo" etc means and Google didn't
; help so I had to look at an answer key :(

(define (carmichael-legit? n)
  (define (is-congruent? n a)
    (cond ((= a 1) #t)
          ((not (= (expmod a n n) a)) #f)
          (else (is-congruent? n (- a 1)))))
  (is-congruent? n (- n 1)))

(carmichael-legit? 561)
(carmichael-legit? 1105)
(carmichael-legit? 1729)
(carmichael-legit? 2465)
(carmichael-legit? 2821)
(carmichael-legit? 6601)
(not (carmichael-legit? 27))

; 1.28

; Really getting tired of the math stuff.
; I looked ahead and it seems like chapters 2+ are more practical.
; If not, I may switch to another book 🥴

(define (mr-check n a)
  (if (and (not (or (= n 1)
                    (= n (- a 1))))
           (= (remainder (square n) a) 1))
      0
      (remainder (square n) a)))

(define (mr-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (mr-check (mr-expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (mr-expmod base (- exp 1) m))
                    m))))

(define (mr-test n)
  (define (mr-test-test n1)
    (= (mr-expmod n1 (- n 1) n) 1))
  (mr-test-test (+ 1 (random (- n 1)))))

(define (is-mr-prime? n times)
  (cond ((= times 0) #t)
        ((mr-test n)
         (is-mr-prime? n (- times 1)))
        (else #f)))

(not (is-mr-prime? 561 100))
(not (is-mr-prime? 1105 100))
(not (is-mr-prime? 1729 100))
(not (is-mr-prime? 27 100))
(is-mr-prime? 167 100)
(is-mr-prime? 229 100)