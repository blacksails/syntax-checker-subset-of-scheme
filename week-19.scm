; Exercise 1
; if called on the empty list then the empty list is returned
; if called on an improper list an exeption is thrown
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
    (= 'cond (car v))))

;;; predicate:
(define is-quasiquote?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'quasiquote))))

;;; 1st accessor:
(define quasiquote-1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;
;;; the syntax checker proper for expressions:
;;;;;;;;;;

(define check-expression
  (lambda (v)
    (cond
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
      [(is-and? v)
       (check-and-expression v)]
      [(is-quasiquote? v)
       (check-quasiquote-expression v)]
      [else
       ;;; (errorf 'check-expression "unrecognized: ~s" v)
       (errorf 'check-expression "not implemented yet: ~s" v)])))

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
             (= '=> (list-ref v 1))
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
                      

(define check-quasiquote-expression
  (lambda (v)
    (letrec ([visit (lambda (v number-of-nestings)
                      (errorf 'check-quasiquote-expression "not implemented yet: ~s" v))])
      (visit (quasiquote-1 v) 0))))

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

(define left-fold-proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws a)
                        (if (null? ws)
                            a
                            (visit (cdr ws) (cons-case (car ws) a))))])
        (visit vs nil-case)))))
 
; (right-fold-proper-list '() cons) constructs a list from a list
; (left-fold-proper-list '() cons) constructs a list from a list starting from the end
