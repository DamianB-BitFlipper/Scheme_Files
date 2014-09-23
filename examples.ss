
;;letrec example
(letrec ([countdown 
          (lambda (x step)
            (begin (display x)
                   (newline)
                   (if (not (zero? x))
                       (countdown (- x step) step)
                       (display "Lift off!\n"))))])
  (countdown 10 0.5))

;;useful function with car and cdr
(define (list-display2 ls)
  (if (null? ls) ;;check if the list is null and exit immediatly if so
      #f
      (begin 
        (display "(") ;;begin by displaying the opening parenthesis and the first element of ls
        (display (car ls))
        (letrec ([rest 
                   (lambda (rest_ls) ;;until rest_ls is not null, display ', <elem>', at the end displays the closing parenthesis
                     (if (not (null? rest_ls))
                         (begin
                           (display ", ")
                           (display (car rest_ls))
                           (rest (cdr rest_ls)))
                         (display ")")))])
          (rest (cdr ls))))))

(define list-display3
  (case-lambda 
   [() #f]
   [(x) (begin
          (display "(")
          (display x)
          (display ")"))]
))

(list-display2 '(1 2 3 4))