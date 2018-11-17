#lang racket
(provide quote #%datum #%app #%top)

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define parsed-lines
    (map (lambda [line] (regexp-match #px"^\\s*(\\d+)\\s*(\\^+)\\s*(\\d+)\\s*$" line))
         (drop src-lines 1)))
  (define datum-lines
    (map (lambda [line index]
                 (if (eq? line #f)
                     `(raise ,(index . + . 2))
                     `(print-up-arrow
                        ,(string->number (list-ref line 1))
                        ,(string-length (list-ref line 2))
                        ,(string->number (list-ref line 3)))))
         parsed-lines
         (range (length parsed-lines))))
  (datum->syntax #f
    `(module up-arrow "up-arrow.rkt"
      ,@datum-lines)))
(provide read-syntax)

(define-syntax-rule (up-arrow-module-begin expr ...)
  (#%module-begin
   expr ...))
(provide (rename-out [up-arrow-module-begin #%module-begin]))

(define (up-arrow operand arrows power)
  (cond
    [(eq? arrows 0)
      (* operand power)]
    [(eq? power 0)
      1]
    [else
      (let ([subresult (up-arrow operand arrows (- power 1))])
        (up-arrow operand (- arrows 1) subresult))]))
(provide up-arrow)

(define (print-up-arrow operand arrows power)
  (let ([res (up-arrow operand arrows power)]
        [expr (format "~a ~a ~a" operand (make-string arrows #\^) power)])
    (printf "~a = ~a\n" expr res)))
(provide print-up-arrow)

(define (raise line-number)
  (printf "Invalid expression on line ~a\n" line-number))
(provide raise)
