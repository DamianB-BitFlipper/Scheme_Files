
(require srfi/1)

(define (delete-n ls n)

  ;;fold uses (index, out_elem, everything else) as its accumulator
  ;; the index is not needed, so cdr the return of fold to get out_elem and everything else
  (cdr
   (fold (lambda (x acc) 
           (let ([index (list-ref acc 0)]
                 [out_elem  (list-ref acc 1)]
                 [rest_elem (list-ref acc 2)])

             ;;split the out_elem, do not add it to rest_elem
             ;;else, append x to rest_elem and increment index
             (if (= index n)
                 (list (add1 index) x rest_elem)
                 (list (add1 index) out_elem (append rest_elem (list x)))))) 

         (list 0 #f '()) ls)))

;;applied items to delete-n using the respective n
(define (tienda* items n)
  (if (= n (length items))
      '()
      (cons (delete-n items n) (tienda* items (add1 n)))))

;;takes (x (a b c ...)) account and returns true if x + a or x + b or x + c equals account
(define (tienda** ls account)
  (let* ([first (car ls)]
         [rest (car (cdr ls))]
         [response (filter (lambda (x) (= (+ first x) account)) rest)])
    (> (length response) 0)))

(define (tienda account items)
  ;;the starting list will be the items processed through tienda*
  (fold (lambda (x acc) 
          ;;if acc is already true do not change it to false accidentally
          ;; else, test with tienda**
          (if (eq? acc #t)
              #t
              (tienda** x account)))
        #f (tienda* items 0)))
  
(tienda 100 '(2 25 75))
