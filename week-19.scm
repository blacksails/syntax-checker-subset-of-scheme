; Exercise 1
; if called on the empty list then the empty list is returned
; if called on an improper list an exception is thrown
; if called with zero parameters then the empty list is returned
; if called with one papameter then the parameter is returned
(define proper-list-append
  (lambda xs
    (letrec ([visit (lambda (xs)
                      (if (null? xs)
                          '()
                          (append (car xs) (visit (cdr xs)))))])
      (visit xs))))

; Exercise 2
(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define check-program
  (lambda (v)
    (cond
      [(null? v)
       #t]
      [(pair? v)
       (and (check-toplevel-form (car v))
            (check-program (cdr v)))]
      [else
       #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define check-toplevel-form
  (lambda (v)
    (cond
      [(is-definition? v)
       (check-definition v)]
      [else
       (check-expression v)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for definitions:
;;;;;;;;;;
;;; predicate:
(define is-definition?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'define))))

;;; 1st accessor:
(define define-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define define-2
  (lambda (v)
    (list-ref v 2)))

;;;;;;;;;;
;;; the syntax checker proper for definitions:
;;;;;;;;;;

(define check-definition
  (lambda (v)
    (and (check-variable (define-1 v))
         (check-expression (define-2 v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for expressions:
;;;;;;;;;;

;;; predicate:
(define is-time?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'time))))

;;; 1st accessor:
(define time-1
  (lambda (v)
    (list-ref v 1)))

;;; predicate:
(define is-if?
  (lambda (v)
    (and (proper-list-of-given-length? v 4)
         (equal? (car v) 'if))))

;;; 1st accessor:
(define if-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define if-2
  (lambda (v)
    (list-ref v 2)))

;;; 3rd accessor:
(define if-3
  (lambda (v)
    (list-ref v 3)))

;;; predicate:
(define is-cond?
  (lambda (v)
    (and (list-strictly-longer-than? v 1)
         (equal? 'cond (car v)))))

;;; predicate:
(define is-case?
  (lambda (v)
    (and (list-strictly-longer-than? v 2)
         (equal? 'case (car v)))))

;;; 1st accessor:
(define case-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define case-2
  (lambda (v)
    (cdr (cdr v))))

;;; predicate:
(define is-and?
  (lambda (v)
    (and (list-strictly-longer-than? v 0)
         (equal? 'and (car v)))))

;;; predicate:
(define is-or?
  (lambda (v)
    (and (list-strictly-longer-than? v 0)
         (equal? 'or (car v)))))

;;; predicate:
(define is-let?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? 'let (car v))
         (list? (list-ref v 1)))))

;;; predicate:
(define is-letstar?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? 'let* (car v)))))

;;; predicate:
(define is-letrec?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? 'letrec (car v))
         (list? (list-ref v 1)))))

;;; predicate:
(define is-begin?
  (lambda (v)
    (and (list-strictly-longer-than? v 1)
         (equal? 'begin (car v)))))

;;; predicate:
(define is-quote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? 'quote (car v)))))

;;; 1st acessor:
(define quote-1
  (lambda (v)
    (list-ref v 1)))

;;; predicate:
(define is-quasiquote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'quasiquote))))

;;; 1st accessor:
(define quasiquote-1
  (lambda (v)
    (list-ref v 1)))

;;; predicate:
(define is-unquote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? 'unquote (car v)))))

;;; 1st accessor:
(define unquote-1
  (lambda (v)
    (list-ref v 1)))

;;; predicate:
(define is-unquote-splicing?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? 'unquote-splicing (car v)))))

;;; 1st accessor:
(define unquote-splicing-1
  (lambda (v)
    (list-ref v 1)))

;;; predicate:
(define is-lambda-abstraction?
  (lambda (v)
    (cond
      [(equal? (car v) 'lambda)
       (proper-list-of-given-length? v 3)]
      [(equal? (car v) 'trace-lambda)
       (proper-list-of-given-length? v 4)]
      [else
       #f])))

;;; 1st accessor:
(define lambda-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define lambda-2
  (lambda (v)
    (list-ref v 2)))

;;; 3rd accessor:      (only used in trace-lambda)
(define lambda-3
  (lambda (v)
    (list-ref v 3)))

;;; predicate:
(define is-application?
  (lambda (v)
    (and (list-strictly-longer-than? v 0)
         (check-expression (car v)))))

;;;;;;;;;;
;;; the syntax checker proper for expressions:
;;;;;;;;;;

(define check-expression
  (lambda (v)
    (cond
      [(null? v)
       #f]
      [(number? v)
       #t]
      [(boolean? v)
       #t]
      [(char? v)
       #t]
      [(string? v)
       #t]
      [(symbol? v)
       (check-variable v)]
      [(is-time? v)
       (check-time-expression v)]
      [(is-if? v)
       (check-if-expression v)]
      [(is-cond? v)
       (check-cond-expression v)]
      [(is-case? v)
       (check-case-expression v)]
      [(is-and? v)
       (check-and-expression v)]
      [(is-or? v)
       (check-or-expression v)]
      [(is-let? v)
       (check-let-expression v)]
      [(is-letstar? v)
       (check-letstar-expression v)]
      [(is-letrec? v)
       (check-letrec-expression v)]
      [(is-begin? v)
       (check-begin-expression v)]
      [(is-quote? v)
       (check-quote-expression v)]
      [(is-quasiquote? v)
       (check-quasiquote-expression v)]
      [(is-lambda-abstraction? v)
       (check-lambda-abstraction v)]
      [(is-application? v)
       (check-application v)]
      [else
       (errorf 'check-expression "unrecognized: ~s" v)])))

(define check-variable
  (lambda (v)
    (not (member v (list 'define 'time 'if 'cond 'else 'case 'and 'or 'let 'let* 'letrec
                         'begin 'quote 'quasiquote 'unquote 'unquote-splicing 'lambda 
                         'trace-lambda)))))

(define check-time-expression
  (lambda (v)
    (check-expression (time-1 v))))

(define check-if-expression
  (lambda (v)
    (and (check-expression (if-1 v))
         (check-expression (if-2 v))
         (check-expression (if-3 v)))))

(define check-cond-clause
  (lambda (v)
    (or (and (proper-list-of-given-length? v 1)
             (check-expression (car v)))
        (and (proper-list-of-given-length? v 2)
             (check-expression (list-ref v 0))
             (check-expression (list-ref v 1)))
        (and (proper-list-of-given-length? v 3)
             (check-expression (list-ref v 0))
             (equal? '=> (list-ref v 1))
             (check-expression (list-ref v 2))))))

(define check-cond-expression
  (lambda (v)
    (letrec ([visit (lambda (v)
                      (if (not (proper-list-of-given-length? v 1))
                          (if (check-cond-clause (car v))
                              (visit (cdr v))
                              #f)
                          (and (proper-list-of-given-length? (car v) 2)
                               (equal? (list-ref (car v) 0) 'else)
                               (check-expression (list-ref (car v) 1)))))])
      (visit (cdr v)))))

(define check-case-expression
  (lambda (v)
    (and
     (check-expression (case-1 v))
     (letrec ([visit (lambda (v)
                       (if (not (proper-list-of-given-length? v 1))
                           (if (and (proper-list-of-given-length? (car v) 2)
                                    (if (list? (list-ref (car v) 0))
                                        (helper-check-case-expression (list-ref (car v) 0))
                                        #f)
                                    (check-expression (list-ref (car v) 1)))
                               (visit (cdr v))
                               #f)
                           (and (proper-list-of-given-length? (car v) 2)
                                (equal? (list-ref (car v) 0) 'else)
                                (check-expression (list-ref (car v) 1)))))])
       (visit (case-2 v))))))

(define helper-check-case-expression
  (lambda (v)
    (letrec ([visit (lambda (v)
                      (if (not (null? v))
                          (if (check-quotation (car v))
                              (visit (cdr v))
                              #f)
                          #t))])
      (visit v))))                              

(define check-and-expression
  (lambda (v)
    (letrec ([visit (lambda (v)
                      (if (not (null? v))
                          (if (check-expression (car v))
                              (visit (cdr v))
                              #f)
                          #t))])
      (visit (cdr v))))) 

(define check-or-expression
  (lambda (v)
    (letrec ([visit (lambda (v)
                      (if (not (null? v))
                          (if (check-expression (car v))
                              (visit (cdr v))
                              #f)
                          #t))])
      (visit (cdr v)))))

(define check-let-expression
  (lambda (v)
    (letrec ([visit (lambda (l vars)
                      (if (not (null? l))
                          (if (and (proper-list-of-given-length? (car l) 2)
                                   (if (symbol? (list-ref (car l) 0))
                                       (check-variable (list-ref (car l) 0))
                                       #f)
                                   (check-expression (list-ref (car l) 1))
                                   (not (member (list-ref (car l) 0) vars)))
                              (visit (cdr l) (cons (list-ref (car l) 0) vars))
                              #f)
                          (check-expression (list-ref v 2))))])
      (visit (list-ref v 1) '()))))

(define check-letstar-expression
  (lambda (v)
    (letrec ([visit (lambda (l)
                      (if (not (null? l))                          
                          (if (and (proper-list-of-given-length? (car l) 2)
                                   (if (symbol? (list-ref (car l) 0))
                                       (check-variable (list-ref (car l) 0))
                                       #f)
                                   (check-expression (list-ref (car l) 1)))
                              (visit (cdr l))
                              #f)
                          (check-expression (list-ref v 2))))])
      (visit (list-ref v 1)))))

(define check-letrec-expression
  (lambda (v)
    (letrec ([visit (lambda (l vars)
                      (if (not (null? l))
                          (if (and (proper-list-of-given-length? (car l) 2)
                                   (if (symbol? (list-ref (car l) 0))
                                       (check-variable (list-ref (car l) 0))
                                       #f)
                                   (check-lambda-abstraction (list-ref (car l) 1))
                                   (not (member (list-ref (car l) 0) vars)))
                              (visit (cdr l) (cons (list-ref (car l) 0) vars))
                              #f)
                          (check-expression (list-ref v 2))))])
      (visit (list-ref v 1) '()))))

(define check-begin-expression
  (lambda (v)
    (letrec ([visit (lambda (v)
                      (if (not (null? v))
                          (if (check-expression (car v))
                              (visit (cdr v))
                              #f)
                          #t))])
      (visit (cdr v)))))

(define check-quotation
  (lambda (v)
    (letrec ([visit (lambda (v)
                      (cond 
                        [(pair? v)
                         (and (visit (car v))
                              (visit (cdr v)))]
                        [(or (number? v)
                             (boolean? v)
                             (char? v)
                             (string? v)
                             (symbol? v)
                             (null? v))
                         #t]
                        [else #f]))])
      (visit v))))

(define check-quote-expression
  (lambda (v)
    (check-quotation (list-ref v 1))))

(define check-quasiquote-expression
  (lambda (v)
    (letrec ([visit (lambda (v number-of-nestings)
                      (cond
                        [(or (number? v)
                             (boolean? v)
                             (string? v)
                             (symbol? v)
                             (null? v))
                         #t]
                        [(is-quasiquote? v)
                         (visit (quasiquote-1 v) (+ 1 number-of-nestings))]
                        [(is-unquote? v)
                         (if (= number-of-nestings 0)
                             (check-expression (quote-1 v))
                             (visit (quote-1 v) (- number-of-nestings 1)))]
                        [(is-unquote-splicing? v)
                         (if (= number-of-nestings 0)
                             (check-expression (unquote-splicing-1 v))
                             (visit (unquote-splicing-1 v) (- number-of-nestings 1)))]
                        [(not (list? v))
                         (if (pair? v)
                             (and (visit (car v) number-of-nestings)
                                  (visit (cdr v) number-of-nestings))
                             #f)]
                        [else
                         #f]))])
      (visit (quasiquote-1 v) 0))))

(define check-lambda-formals
  (lambda (v)
    (cond
      [(symbol? v)
       (check-variable v)]
      [(list? v)
       (letrec ([visit (lambda (v vars)
                         (if (not (null? v))
                             (if (symbol? (car v))
                                 (if (and (check-variable (car v))
                                          (not (member (car v) vars)))
                                     (visit (cdr v) (cons (car v) vars))
                                     #f)
                                 #f)
                             #t))])
         (visit v (list)))]
      [else ; pair? = #t
       (letrec ([visit (lambda (v vars)
                         (if (pair? v)
                             (if (symbol? (car v))
                                 (if (and (check-variable (car v))
                                          (not (member (car v) vars)))
                                     (visit (cdr v) (cons (car v) vars))
                                     #f)
                                 #f)
                             (if (symbol? v)
                                 (and (check-variable v)
                                      (not (member v vars)))
                                 #f)))])
             (visit v (list)))])))

(define check-lambda-abstraction
  (lambda (v)
    (cond
      [(equal? (car v) 'lambda)
       (and (check-lambda-formals (list-ref v 1))
            (check-expression (list-ref v 2)))]
      [(equal? (car v) 'trace-lambda)
       (and (check-quotation (list-ref v 1))
            (check-lambda-formals (list-ref v 2))
            (check-expression (list-ref v 3)))]
      [else
      #f])))

(define check-application
  (lambda (v)
    (letrec ([visit (lambda (v)
                      (if (not (null? v))
                          (if (check-expression (car v))
                              (visit (cdr v))
                              #f)
                          #t))])
      (visit v))))

;;;;;;;;;;
;;; auxiliaries:
;;;;;;;;;;

(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (and (pair? v)
                           (or (= i 0)
                               (visit (cdr v)
                                      (- i 1)))))])
      (if (>= n 0)
          (visit v n)
          (errorf 'list-strictly-longer-than? "negative length: ~s" n)))))

;;; reads an entire file as a list of Scheme data
;;; use: (read-file "filename.scm")
(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit (lambda ()
                          (let ([in (read p)])
                            (if (eof-object? in)
                                '()
                                (cons in (visit)))))])
          (visit))))))

;;; interface:
(define check-file
  (lambda (filename)
    (if (string? filename)
        (check-program (read-file filename))
        (errorf 'check-file "not a string: ~s" filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 3
(define curry3
  (lambda (f)
    (lambda (x1)
      (lambda (x2)
        (lambda (x3)
        (f x1 x2 x3))))))

(define uncurry3
  (lambda (f)
    (lambda (x1 x2 x3)
      (((f x1) x2) x3))))

; Exercise 5
(define right-fold-proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws)
                        (if (null? ws)
                            nil-case
                            (cons-case (car ws)
                                       (visit (cdr ws)))))])
        (visit vs)))))
; (right-fold-proper-list '() cons) constructs a list from a list. By making pairs of an entity and a list, 
; exactly as the normal (list ...) procedure. It becomes a proper list because the nil-case is the empty list.
; We could call this the identity function.

(define left-fold-proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws a)
                        (if (null? ws)
                            a
                            (visit (cdr ws) (cons-case (car ws) a))))])
        (visit vs nil-case)))))
; (left-fold-proper-list '() cons) constructs a list from a list starting from the the entity appearing as
; the first in the list. Which means that that the list is reversed. But the cdr of the inner most pair remains 
; as the empty list, as a starts being '(). Therefore this is still a proper list. And the function is the
; reverse function
