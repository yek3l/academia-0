(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (if (null? rests) nil
    (map (lambda (x) (cons first x)) rests)
  )
)

(define (zip pairs)
  (if (null? (car pairs)) nil
      (cons (map car pairs) (zip (map cdr pairs))))  
)

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enumerator s i)
    (if (null? s) nil
      (cons (list i (car s)) (enumerator (cdr s) (+ i 1)))
    )
  )
  (enumerator s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((= total 0) (list nil)) ;there IS a way to make 0, it's just using NONE of the coins
    ((null? denoms) nil)
    ((< total 0) nil)
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms)) 
                  (list-change total (cdr denoms)))
    )
  )
)
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         ; expr !!!
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
          expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons 'lambda (cons params (map let-to-lambda body))) 
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr))) 
           ; BEGIN PROBLEM 19 -- WOW IF WE HAVE TIME LET US MAKE THIS NICER BC WOW WHAT IS ABSTRACTION??
           (define args (car (zip values)))
           (define inputs (let-to-lambda (cadr (zip values))))
           (cons (cons 'lambda (cons args (let-to-lambda body))) inputs)
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))






