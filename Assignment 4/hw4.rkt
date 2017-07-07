
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x)(string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]));;(list-ref xs (remainder n (length xs))) is better

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)])
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))          
  
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- 0 x) (lambda () (f (+ x 1))));; (- x) is okey.
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda ()(f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (string=? x "dan.jpg")
                    (cons "dan.jpg" (lambda () (f "dog.jpg")))
                    (cons "dog.jpg" (lambda () (f "dan.jpg")))))])
     (lambda ()(f "dan.jpg"))))

(define (stream-add-zero s)  
  (letrec ([f (lambda (s)
                (let ([next (s)]) 
                  (cons (cons 0 (car next)) (lambda () (f (cdr next))))))])
    (lambda () (f s)))) 

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([lvec (vector-length vec)]
           [f (lambda (it)
                (if (= it lvec)
                    #f                    
                    (let ([cnt (vector-ref vec it)])
                      (if (and (pair? cnt) (equal? v (car cnt)))
                          cnt
                          (f (+ it 1))))))])                  
    (f 0)))

(define (cached-assoc xs n)
  (let([memo (make-vector n #f)]
         [pos 0])
  (lambda (v)
    (let([oldans (vector-assoc v memo)])
      (if oldans
          oldans
          (let([newans (assoc v xs)])
            (if newans
                (begin (vector-set! memo pos newans)
                       (if (= pos (- n 1))
                           (set! pos 0)
                           (set! pos (+ pos 1)))
                       newans)
                newans)))))))
                
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec([y e1]
          [f (lambda ()
               (let([x e2])
                (if (or (not (number? x)) (>= x y))
                   #t
                   (f))))])
       (f))]))
        
   
             
                    
                    