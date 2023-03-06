#lang racket
; --------------------------------- ;
; ------- Scanner Functions ------- ;
; --------------------------------- ;
(define (read-file file)
  (file->lines file))

(define (tokenize str)
  (cond
    [(idx? str) 'idx]
    [(id? str) 'id]
    [else
     (let
         ([val1 (operator? str)])
       (if (not (equal? val1 #f))
           val1
           (let
               ([val2 (keyword? str)])
             (if (not (equal? val2 #f))
                      val2
                      'err))))]))
          
  
(define (idx? str)
  (if (string->number str)
      (when (> (string->number str) 0) #t)
      #f))

(define (id? str)
  (if (string-alphabetic? str)
      (if (not (keyword? str))
          #t
          #f) ;returns false here when the string is alphabetic but its a keyword
      #f)) ;returns false here if the string isn't alphabetic

(define (operator? str)
  (cond
    [(equal? str "(") 'lparen]
    [(equal? str ")") 'rparen]
    [(equal? str "+") 'plus]
    [(equal? str "-") 'minus]
    [(equal? str "=") 'equals]
    [(equal? str ":") 'colon]
    [(equal? str "$$") 'eof]
    [else #f]))


(define (string-alphabetic? str)
   (define (iter ch-list)
       (if (empty? ch-list)
          #t ; made it to the end, no non-alphas
          (if (char-alphabetic? (first ch-list))
              (iter (rest ch-list))
              #f))) ; found non-alpha, bail out
     ;function body (1 line, sets up call to iterator function) 
     (iter (string->list str)))

(define (keyword? str)
  (if (string-alphabetic? str)
      (cond
        [(equal? str "goto") 'goto]
        [(equal? str "read") 'read]
        [(equal? str "write") 'write]
        [(equal? str "gosub") 'gosub]
        [(equal? str "return") 'return]
        [(equal? str "if") 'if]
        [(equal? str "then") 'then]
        [else #f])
        #f))

;----------------------------------- ;
; -------- Parser Functions -------- ;
;----------------------------------- ;

(define (program? list)
  (cond
    [(equal? (first (reverse list)) '("$$")) (linelist? (remove '("$$") list))]
    [else (display "Not a valid program, missing '$$' on the final line.")]))



(define (linelist? list)
  (cond
    [(empty? list) (display "Accept")]
    [(line? (first list)) (linelist? (rest list))]
    [else (if (string->number (first (first list)))
              (format "error on line #~a"(first (first list)))
              (format "line starting with #~a"(first (first list))))]))


(define (line? list)
  (let ([list (map tokenize list)])
    (cond
      [(member 'err list) (display "Scan error, invalid token: ")#f]
      [(not (equal? (first list) 'idx)) (display "Syntax error, missing line number.")#f]
      [(equal? (first list) 'idx) (if (not (member 'colon list))
                                    (stmt? (rest list))
                                    (if (stmt? (rest (remove 'colon (reverse (member 'colon (reverse list))))))
                                        (if (linetail? (member 'colon list))
                                            #t
                                            #f)
                                        #f))]
    [else #f])))
    
  
(define (linetail? list)
  (cond
    [(empty? list) #t]
    [(equal? (first list) 'colon) (stmt? (rest list))]
    [else #f])) 


(define (stmt? list)
  (cond
    [(empty? list) #f]
    [(and (equal? (first list) 'id) (equal? (first (rest list)) 'equals)) (expr? (rest (rest list)))]
    [(and (equal? (first list) 'read) (= (length list) 2)(equal? (first (rest list)) 'id)) #t]
    [(equal? (first list) 'write) (expr? (rest list))]
    [(and (equal? (first list) 'goto) (= (length list) 2) (equal? (first (rest list)) 'idx)) #t]
    [(and (equal? (first list) 'gosub)(= (length list) 2) (equal? (first (rest list)) 'idx)) #t]
    [(equal? (first list) 'return) #t]
    [(equal? (first list) 'if) (if (member 'then list)
                                   (cond
                                     [(expr? (rest (remove 'then (reverse (member 'then (reverse list)))))) (stmt? (rest (member 'then list)))]
                                     [else #f])
                                   #f)]
    [else (display "Syntax error, invalid statement: ") #f]))


(define (expr? list)
  (cond
    [(equal? (first list) 'id) (etail? (rest list))]
    [(equal? (first list) 'idx) (etail? (rest list))] ;add support for num instead of index
    [(and (equal? (first list) 'lparen) (equal? (last list) 'rparen)) (expr? (reverse (rest (reverse (rest list)))))] ;This may not be working correctly
    [else (display "Syntax error, invalid expression: ") #f]
  ))

(define (etail? list)
  (cond
    [(empty? list) #t] 
    [(equal? (first list) 'plus) (expr? (rest list))]
    [(equal? (first list) 'minus) (expr? (rest list))]
    [(equal? (first list) 'equals) (expr? (rest list))]
    [else #f]))

;---------------------------------------- ;
; -------- Main Parsing Function -------- ;
;---------------------------------------- ;

(define (parse file)
  (program? (map string-split (read-file file))))






