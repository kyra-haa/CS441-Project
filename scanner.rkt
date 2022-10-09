#lang racket

(require "parser.rkt")


(struct exn:scanner-error exn:fail ())

(define (raise-scanner-error)
  (raise (exn:scanner-error
          (format "incorrect syntax")
          (current-continuation-marks))))


(define (get-file user_input)
  ;get file contents and return a string of tokens seperated by whitespace
  (if (file-exists? user_input)
      (begin
        (flatten
         (for/list ([line (file->lines user_input)])
           (string-split (string-downcase line)))))
      (begin
      (writeln "File not found. Restart program and try again.")
      (exit))))

(define (string-numeric? s)
  ;checks if entire string is numeric
  ;e, f, d can be used as exponents in numbers 
  (cond [(string-contains? s "e")
         #f]
        [(string-contains? s "f")
         #f]
        [(string-contains? s "d")
         #f]
        [else (number? (string->number s 10))]
        ))

(define (string-alpha-or-num? s start result)
  ;checks if each char is alphabetical or numeric 
  (if (not (non-empty-string? s))
      result
      (begin 
        (let ([current (substring s start (+ start 1))])
          (if (or (char-alphabetic? (first(string->list current))) (char-numeric? (first(string->list current))))
              (begin
                (string-alpha-or-num? (substring s (+ start 1)) start (string-append current result)))
              result)))))

(define (reverse-string s)
  ;reverses string 
  (list->string (reverse (string->list s))))

(define (string-id? s)
  ;checks if string matches rules to be a string 
  (cond [(char-alphabetic?(string-ref s 0))
         (begin
           (let([result (reverse-string(string-alpha-or-num? s 0 ""))])
             (cond [(string=? "read" result)
                    "read"]
                   [(string=? "write" result)
                    "write"]
                   [else result])))]))

(define (get-num s result)
  ;removes numerical portion of string leaving acceptable symbols to follow 
  (when (non-empty-string? s)
    (let ([a (string-ref s 0)])
      (cond [(char-numeric? a)
             (get-num (substring s 1) (string-append (substring s 0 1) result))]
            [(member a '(#\( #\) #\+ #\- #\* #\/))
             result]
            [(char-alphabetic? a)
             (raise-scanner-error)]))))
           

(define (r-token input_list result_list)
  ;recursive function to make a list of tokens from input 
  (if (null? input_list)
      result_list
      (begin
        (if (non-empty-string? (first input_list))
            (begin
              (let ([item (first input_list)])
                (let([ a (string-ref item 0)])
  
                  (cond [(member item  '("(" ")" "+" "-" "*" "/")) ;entire token is acceptable symbol 
                         (r-token (rest input_list) (cons item result_list))]
                        [(member a '(#\( #\) #\+ #\- #\* #\/)) ;first char of token is acceptable symbol 
                         (r-token (cons (substring item 1) (rest input_list)) (cons (substring item 0 1) result_list))]
                        [(string-numeric? item) ;entire token is number
                         (r-token (rest input_list) (cons "number" result_list))]
                        [(char=? a #\:) ;assignment op
                         (begin (let([ b (string-ref item 1)])
                                  (if (char=? b #\=)
                                      (r-token (cons (substring item 2) (rest input_list)) (cons ":=" result_list))
                                      (raise-scanner-error))))]
                        [(char=? a #\$) ;end of file 
                         (begin (let([ b (string-ref item 1)])
                                  (if (char=? b #\$)
                                      (r-token (cons (substring item 2) (rest input_list)) (cons "$$" result_list))
                                      (raise-scanner-error))))]
                        [(char-numeric? a) ;first char is numeric 
                         (let ([num_result (get-num item "")])
                           (r-token (cons (substring item (string-length num_result)) (rest input_list)) (cons "number" result_list)))]
                        [(when (char-alphabetic? a) ;first char is alphabetic (checks for id, read, and write)
                           (let ([id_result (string-id? item)])
                             (cond [(eqv? id_result "read")
                                    (r-token (rest input_list) (cons "read" result_list))]
                                   [(eqv? id_result "write")
                                    (r-token (rest input_list) (cons "write" result_list))]
                                   [else
                                    (r-token (cons (substring item (string-length id_result)) (rest input_list)) (cons "id" result_list))]
                                   )))]))))
            (r-token (rest input_list) result_list)))))


(define (parse)
  ;function to get user input and begin parsing
  (with-handlers ([exn:scanner-error? (λ (exn) "No")]) ;catches scanner-error and prints "No"
    (writeln "Enter file name to parse:")
    (let ([input (read-line (current-input-port) 'any)])
      (writeln "Does your file pass the test?")
      (parse-start (reverse (r-token (get-file input) '())))
      )))
        

(parse)

