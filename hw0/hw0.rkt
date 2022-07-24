#lang plait

(define (f x)
  (+ x 1))

;part 1 - function definition, call
(define (3rd-power x)
  (* (* x x) x))

(test (3rd-power 17)
      4913)


;part 2 - recursive
(define (n-nd-power n x)
  (cond
    [(= n 0) 1]
    [else (* x (n-nd-power (- n 1) x))]))

(define (42nd-power x)
  (n-nd-power 42 x))

(test (42nd-power 17)
      4773695331839566234818968439734627784374274207965089)


;part 3 - string built-ins
(define (get-last-char str)
  (string-ref str (- (string-length str) 1)))

(define (plural str): String
  (cond
    [(equal? (get-last-char str) #\y) (string-append
                                       (substring str 0 (- (string-length str) 1))
                                       "ies")]
    [else (string-append str "s")]
    ))

(test (plural "baby")
      "babies")
(test (plural "fish")
      "fishs")


;part 4 - type
(define-type Light
  (bulb [watts : Number]
        [technology : Symbol])
  (candle [inches : Number]))

(define (energy-usage (light : Light)) : Number
  (type-case Light light
    [(bulb w t) (/ (* w 24) 1000)]
    [(candle i) 0]))

(test (energy-usage (bulb 100.0 'halogen))
      2.4)

(test (energy-usage (candle 10.0))
      0.0)

;part 5
(define (use-for-one-hour (light : Light)) : Light
  (type-case Light light
    [(bulb w t) light] ; return itself
    [(candle i) (cond
                  [(<= i 1.0)
                   (if (>= i 0.0)
                       (candle 0.0)
                       (error 'TypeError
                              "Candle's inches should be greater or equal than 0.0"))]
                  [else (candle (- i 1.0))])
     ]))

(test (use-for-one-hour (bulb 100.0 'halogen)) 
  (bulb 100.0 'halogen))

(test (use-for-one-hour (candle 10.0))
  (candle 9.0))

(test (use-for-one-hour (candle 0.0))
  (candle 0.0))

(test (use-for-one-hour (candle 0.5))
  (candle 0.0))

; TypeError
; (use-for-one-hour (candle -1))

