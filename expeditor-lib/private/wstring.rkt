#lang racket/base
(require "terminal.rkt")

(provide char-width
         string-width
         string-fits-width)

(define (string-width s [start 0])
  (for/sum ([c (in-string s start)])
    (char-width c)))

(define (string-fits-width str i width)
  (define len (string-length str))
  (let loop ([i i] [width width] [n 0])
    (cond
      [(= i len) n]
      [(zero? width) n]
      [else
       (define w (char-width (string-ref str i)))
       (if (w . > . width)
           n
           (loop (add1 i) (- width w) (add1 n)))])))

  
