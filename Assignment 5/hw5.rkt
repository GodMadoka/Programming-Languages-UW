;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rs)
  (if (null? rs)
      (aunit)
      (apair (car rs) (racketlist->mupllist (cdr rs)))))
(define (mupllist->racketlist ms)
  (if (aunit? ms)
      null
      (cons (apair-e1 ms) (mupllist->racketlist (apair-e2 ms)))))
      
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; Do add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e) (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let ([f (closure-fun v1)])
                 (eval-under-env (fun-body f)
                                 (if (fun-nameopt f)
                                     (cons (cons (fun-nameopt f) v1) (cons (cons (fun-formal f) v2) (closure-env v1)))
                                     (cons (cons (fun-formal f) v2) (closure-env v1)))))
               (error "MUPL call applied to non-function")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))
  
(define (ifeq e1 e2 e3 e4)
  (mlet  "_x" e1
         (mlet "_y" e2
               (ifgreater (var "_x") (var "_y")
                          e4
                          (ifgreater (var "_y") (var "_x")
                                     e4
                                     e3)))))

;; Problem 4

(define mupl-map
  (fun "mmap" "mf"
       (fun #f "mlist"
            (ifaunit (var "mlist")
                     (aunit) 
                     (apair (call (var "mf") (fst (var "mlist"))) (call (call (var "mmap") (var "mf")) (snd (var "mlist"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (fun #f "mlist"
                  (call (call (var "map") (fun #f "y" (add (var "i") (var "y")))) (var "mlist"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
 (struct e-with-env (e env));; store a expression with its env
 (define (build-e-with-env e)
  (cond [(var? e) (e-with-env e (set (var-string e)))]
         [(int? e) (e-with-env e (set))]
         [(add? e) (let ([v1 (build-e-with-env (add-e1 e))]
                         [v2 (build-e-with-env (add-e2 e))])
                     (e-with-env (add (e-with-env-e v1) (e-with-env-e v2))
                          (set-union (e-with-env-env v1) (e-with-env-env v2))))]
         [(aunit? e) (e-with-env e (set))]
         [(closure? e) (e-with-env e (set))]
         [(fun? e) (let* ([v1 (build-e-with-env (fun-body e))]
                          [env (set-remove (e-with-env-env v1) (fun-formal e))]
                          [env (if (fun-nameopt e)
                                   (set-remove env (fun-nameopt e))
                                   env)])
                     (e-with-env (fun-challenge (fun-nameopt e) (fun-formal e) (e-with-env-e v1) env) env))]
         [(ifgreater? e) (let ([v1 (build-e-with-env (ifgreater-e1 e))]
                               [v2 (build-e-with-env (ifgreater-e2 e))]
                               [v3 (build-e-with-env (ifgreater-e3 e))]
                               [v4 (build-e-with-env (ifgreater-e4 e))])
                           (e-with-env (ifgreater (e-with-env-e v1) (e-with-env-e v2) (e-with-env-e v3) (e-with-env-e v4))
                                (set-union (e-with-env-env v1) (e-with-env-env v2) (e-with-env-env v3) (e-with-env-env v4))))]
         [(mlet? e) (let ([v1 (build-e-with-env (mlet-body e))]
                          [v2 (build-e-with-env (mlet-e e))])
                      (e-with-env (mlet (mlet-var e) (e-with-env-e v2) (e-with-env-e v1)) (set-union (e-with-env-env v2) (set-remove (e-with-env-env v1) (mlet-var e)))))]
         [(call? e) (let ([v1 (build-e-with-env (call-funexp e))]
                          [v2 (build-e-with-env (call-actual e))])
                      (e-with-env (call (e-with-env-e v1) (e-with-env-e v2)) (set-union (e-with-env-env v1) (e-with-env-env v2))))]
         [(apair? e) (let ([v1 (build-e-with-env (apair-e1 e))]
                           [v2 (build-e-with-env (apair-e2 e))])
                       (e-with-env (apair (e-with-env-e v1) (e-with-env-e v2)) (set-union (e-with-env-env v1) (e-with-env-env v2))))]
         [(fst? e) (let ([v1 (build-e-with-env (fst-e e))])
                     (e-with-env (fst (e-with-env-e v1)) (e-with-env-env v1)))]
         [(snd? e) (let ([v1 (build-e-with-env (snd-e e))])
                     (e-with-env (snd (e-with-env-e v1)) (e-with-env-env v1)))]
         [(isaunit? e) (let ([v1 (build-e-with-env (isaunit-e e))])
                         (e-with-env (isaunit (e-with-env-e v1)) (e-with-env-env v1)))]
         [#t (error (format "bad MUPL expression ~v" e))]))
  (e-with-env-e (build-e-with-env e)))
                         
          

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e) (lambda (s) (cons s (envlookup env s)))) e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e) (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e) (eval-under-env-c (mlet-e e) env)) env))]
        [(call? e)
         (let ([v1 (eval-under-env-c (call-funexp e) env)]
               [v2 (eval-under-env-c (call-actual e) env)])
           (if (closure? v1)
               (let ([f (closure-fun v1)])
                 (eval-under-env-c (fun-challenge-body f)
                                   (if (fun-challenge-nameopt f)
                                       (cons (cons (fun-challenge-nameopt f) v1) (cons (cons (fun-challenge-formal f) v2) (closure-env v1)))
                                       (cons (cons (fun-challenge-formal f) v2) (closure-env v1)))))
               (error "MUPL call applied to non-function")))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v1 (eval-under-env-c (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v1 (eval-under-env-c (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([v1 (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v1)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
