
;;strings
(require srfi/13)

(define (space-append ls)
  (let ([firsts (drop-right ls 1)]
        [last (take-right ls 1)])
    ;;on all strings in first, append a " " to the end and then concatonate all off them allong with the last
    (string-concatenate (append (map (lambda (x) (string-append x " ")) firsts) last))))

(define (reversar-palabras palabras)
  (space-append (reverse (string-split palabras " "))))

(display (reversar-palabras "Hola amigo! Que tal!")) (newline)

(define (do-magic fun)
  (map fun (string->list "HELLO")))

(do-magic (lambda (x) (display (make-list 2 x))))

