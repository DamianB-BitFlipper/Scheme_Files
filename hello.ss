#|
(define (square_root* x guess iters)
  (begin
    (if (< iters 10) ;;work for 10 times
        (square_root* x 
                      (/ (+ (/ x guess) guess) 2) 
                      (+ iters 1))
        (exact->inexact guess)))) ;;change guess from an exact to an inexact

(define (square_root x)
  (square_root* x 1 0))

(display (square_root 2))

#;(let row ([i 10] [b 2])
    (if (or (= i 0) (= b 1))
        (begin    
          (newline)
          (display i b))
        (row (- i 1) (- b 1))))

;(letrec ((helper (lambda (x)))))

(let sum ((x 10))
  (lambda (x) 
    (if (= x 0) x (+ x (sum (- x 1))))))

|#

;;
;;Problems from http://community.schemewiki.org/?ninety-nine-scheme-problems
;;

;;fold and such libs
(require srfi/1)

;;list comprehension
(require srfi/42)

;;Problem 1
;;case-lambda (not working yet)
(define problem1
  (case-lambda
   [() #f]
   [(x) x]
   [ls (problem1 (cdr ls))]))

(define (problem1* ls)
  (cond [(null? ls) '()]                ;;list must have at least 1 element
        [(= (length ls) 1) (car ls)]    ;;if the list has 1, return that 1
        [else (problem1* (cdr ls))]))   ;;re-pass the cdr of ls

(display (problem1 '(1 2 3 4 5))) (newline)
(display (problem1* '(1 2 3 4 5))) (newline)

;;Problem 2
(define (problem2 ls)
  (cond [(< (length ls) 2) #f]          ;;list must have atleast 2 elements
        [(= (length ls) 2) (car ls)]    ;;once the list has 2 elements, return the first of those 2
        [else (problem2 (cdr ls))]))

(display (problem2 '(1 2 3 4 5))) (newline)

;;Problem 3
(define (problem3 ls x)
  (cond [(> x (- (length ls) 1)) #f] ;;sanity check
        [(zero? x) (car ls)]         ;;if we reached x to 0, the first elem of ls is the one that is wanted
        [else (problem3 (cdr ls) (- x 1))]))

(display (problem3 '(1 2 3 4 5) 2)) (newline)

;;Problem 4
;;letrec example
(define (problem4 ls)
  (letrec ([p4 (lambda (ls i)
                 (if (null? ls) i 
                     (p4 (cdr ls) (+ i 1))))])
    (p4 ls 0)))

(define (problem4* ls) 
  (fold (lambda (x acc) (+ acc 1)) 0 ls))

(display (problem4 '(1 2 3 4 5 6 7))) (newline)
(display (problem4* '(1 2 3 4 5 6 7))) (newline)

;;Problem 5
;;cons example
(define (problem5 ls)
  (fold (lambda (x acc) (cons x acc)) '() ls))

(display (problem5 '(1 2 3 4 5 6))) (newline)

;;Problem 6
(define (problem6 ls)
  (equal? ls (problem5 ls))) ;;problem5 reverses a list

(display (problem6 '(1 2 3 3 2 1))) (newline)

;;Problem 7
;;let example
(define (atom? x) (not (or (pair? x) (null? x) (vector? x))))

(define (problem7 ls)
  (if (null? ls) '()  ;;sanity check for empty list
      (let ([first (car ls)])
        (cond [(atom? first) (cons first (problem7 (cdr ls)))] ;;if the first element of ls is an atom
              [else (append (problem7 first) (problem7 (cdr ls)))])))) ;;if the first element is not an atom

(display (problem7 '(1 2 3 (2 (5 6 7) 3) (5 6)))) (newline)

;;Problem 8
(define (problem8 ls)
  (fold (lambda (x acc) (if (eq? (problem1* acc) x) acc
                            (append acc (list x)))) ;;problem1* get the last element in the list, if x is the same as the last element in acc dont add it to the end
        '() ls))

(display (problem8 '(1 1 1 2 2 3 4 4 5))) (newline)

;;Problem 9
(define (problem9 ls)
  (let ([ils (map list ls)])
    (fold (lambda (x acc) (if (equal? (problem1* (problem1* acc)) (problem1* x)) ;;should be able to be made better, of that last element (a list) of acc, take its last (the value) and compare it to the last of x (its value)
                              (append (drop-right acc 1) (list (append x (problem1* acc)))) ;;if they match, remove the last value (list) from acc and append the x ++ last value
                              (append acc (list x)))) ;;append x as to the end of acc as a list
          '() ils)))

(display (problem9 '(1 1 1 1 2 3 4 4 5 6))) (newline)

;;Problem 10
(define (problem10 ls)
  (map (lambda (e) (list (length e) (car e))) (problem9 ls))) ;;take the groupings from problem 9 and map through it replacing each group with (list (length group) (car group))

(display (problem10 '(a a a a b c c a a d e e e e))) (newline)

;;Problem 11
(define (problem11 ls)
  (map (lambda (e) (if (= (length e) 1) (car e) ;;basically the same as problem 10
                       (list (length e) (car e))))
       (problem9 ls)))

(display (problem11 '(a a a a b c c a a d e e e e))) (newline)

;;Problem 12
;;make-list example
(define (problem12* ls) ;;expands (4 a) to (a a a a)
  (let ([first (car ls)]
        [second (car (cdr ls))])
    (make-list first second)))

(define (problem12 ls)
  (append-map (lambda (e) (if (atom? e) (list e) (problem12* e)))
       ls))

(display (problem12 '((4 a) b (2 c) (2 a) d (4 e)))) (newline)

;;Problem 13
(define (problem13* ls i last_ele) ;;counts until index i at ls is different from last_ele
 (if (and (< i (length ls)) (eqv? (list-ref ls i) last_ele)) ;;the i <-> length comparision is to prevent overflow
     (problem13* ls (+ i 1) last_ele)
     (if (= i 1) last_ele (list i last_ele)))) ;;once that index is diffent, return the output as wanted

;;drop example
(define (problem13 ls)
  (if (null? ls) '() ;;until there is nothing left of ls, keep on recursing
      (let ([to_insert (problem13* ls 0 (car ls))]) ;;get the data to be inserted
               (if (atom? to_insert) (cons to_insert (problem13 (drop ls 1))) ;;if inserting atom, drop 1 from ls
                   (cons to_insert (problem13 (drop ls (car to_insert)))))))) ;;if not, read the first value and drop that much

(display (problem13 '(a a a a b c c a a d e e e e))) (newline)

;;Problem 14
(define (problem14 ls)
  (fold (lambda (x acc) (append acc (list x x))) '() ls))

(display (problem14 '(a b c c d))) (newline)

;;Problem 15
;;list-ec example
(define (problem15 ls n)
  (fold (lambda (x acc) (append acc (list-ec (: i n) x))) '() ls))

(display (problem15 '(a b c) 3)) (newline)

;;Problem 16
;;zip, remove, unzip2, let-values example
(define (problem16 ls n)
  (let-values ([(first second) ;;unzip and extract values
                (unzip2 
                 (remove (lambda (elem) (= 0 (modulo (car elem) n))) ;;remove based on the index zipped modulo
                         (zip (list-ec (: x 1 (+ (length ls) 1)) x) ls)))]) ;;zip indexing each element from 1 ..
    second))

(display (problem16 (list-ec (: x 15) x) 3)) (newline)

;;Problem 17
;;take and drop example
(define (problem17 ls n)
  (list (take ls n) (drop ls n)))

(display (problem17 '(a b c d e) 3)) (newline)

;;Problem 18
;;drop-right example
(define (problem18 ls start end)
  (drop (drop-right ls (- (length ls) end)) (- start 1)))

(display (problem18 '(a b c d e f g h i k) 3 7)) (newline)

;;Problem 19
;;match example
(define (problem19 ls n)
  (if (< n 0) 
      (match (problem17 ls (+ (length ls) n))
             [(list start end) (append end start)])
      (match (problem17 ls n)
             [(list start end) (append end start)])))

(display (problem19 '(a b c d e f g h) 3)) (newline)
(display (problem19 '(a b c d e f g h) -2)) (newline)

;;Problem 20
(define (problem20 ls n)
  (if (< n 0)
      (reverse (problem20 (reverse ls) (- n)))
      (if (> n (length ls)) ls
          (append (take ls (- n 1)) (drop ls n)))))

(display (problem20 '(a b c d) -2)) (newline)


(display "======================== OTHER ========================") (newline)

;;Multi arg example
(define (max . l)
  (fold (lambda (x acc) (if (> x acc) x acc)) (car l) l))

(display (max 1 2 3 4 5 4 3 2 1)) (newline)

(define (get_names names n)
  (display "names")
  #;(let-values ([(first second) names])
    (begin 
      (display first) (newline)
      (display second) (newline))))

(get_names (values 'John 'Smith))
