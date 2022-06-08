#lang r5rs

(#%require (only racket/base error))

(#%provide position)

;position ADT: describes how positions are saved
(define (position)
  
  ;makes a new position (= a vector with an x or y value)
  (define (new x y)
    (let ((v (make-vector 2)))
      (vector-set! v 0 x)
      (vector-set! v 1 y)
      v))

  ;gives the x or y value of the position
  (define (x vec)
    (vector-ref vec 0))
  (define (y vec)
    (vector-ref vec 1))

  (define (pos=? vec1 vec2)
    (and (= (x vec1) (x vec2))
         (= (y vec1) (y vec2))))
  
  ;changes the x or y value of the position
  (define (set-x! vec x)
    (vector-set! vec 0 x))
  (define (set-y! vec y)
    (vector-set! vec 1 y))
  
  (define (dispatch msg)
    (cond ((eq? msg 'new) new)
          ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'pos=?) pos=?)
          ((eq? msg 'set-x!) set-x!)
          ((eq? msg 'set-y!) set-y!)
          (else (error ": not supported: ") msg)))
    dispatch)