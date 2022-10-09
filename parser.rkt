#lang racket

(provide parse-start)
(provide struct exn:parse-error)

(struct exn:parse-error exn:fail ())

(define (raise-parse-error)
  (raise (exn:parse-error
          (format "incorrect syntax")
          (current-continuation-marks))))
;https://beautifulracket.com/explainer/errors-and-exceptions.html


(define (good-list? list)
  ;checks if item is a non empty list 
  (and (list? list) (not (null? list))))

(define (match2 expected list)
  ;matches expected token to actual token 
  (let ([token (first list)])
    (if (string=? expected token)
        (rest list)
        (raise-parse-error))))


(define (parse-start list)
  ;starts parsing process 
  (with-handlers ([exn:parse-error? (λ (exn) "No")]) ;catches parse-error and prints "No"
    (let ([token (first list)])
      (if (member token '("id" "read" "write" "$$"))
          (begin
            (let ([new_list (stmt-list list)])
              (if (list? new_list)
                  (begin
                    (let ([new_token (first new_list)])
                      (match2 "$$" new_list)
                      "Yes")) ;if successful, prints "Yes"
                  (raise-parse-error))))
          (raise-parse-error)))))

(define (stmt-list list)
  (when (good-list? list)
    (let ([token (first list)])
      (cond [(member token '("id" "read" "write"))
             (stmt-list (stmt list))]
            [(string=? "$$" token)
             list]
            [else (raise-parse-error)]))))


(define (stmt list)
  (when (good-list? list)
    (let ([token (first list)])
      (cond [(string=? "id" token)
             (expr (match2 ":=" (match2 "id" list)))]
            [(string=? "read" token)
             (match2 "id" (match2 "read" list))]
            [(string=? "write" token)
             (expr (match2 "write" list))]
            [else (raise-parse-error)]))))

(define (expr list)
  (when (good-list? list)
    (let ([token (first list)])
      (if (member token '("id" "number" "("))
          (begin (term-tail (term list)))
          (raise-parse-error)))))

(define (term-tail list)
  (when (good-list? list)
    (let ([token (first list)])
      (cond [(member token '("+" "-"))
             (term-tail (term (add-op list)))]
            [(member token '(")" "id" "read" "write" "$$"))
             list]
            [else (raise-parse-error)]))))

(define (term list)
  (when (good-list? list)
    (let ([token (first list)])
      (if (member token '("id" "number" "("))
          (begin (factor-tail (factor list)))
          (raise-parse-error)))))

(define (factor-tail list)
  (when (good-list? list)
    (let ([token (first list)])
      (cond [(member token '("*" "/"))
             (factor-tail (factor (mult-op list)))]
            [(member token '("+" "-" ")" "id" "read" "write" "$$"))
             list]
            [else (raise-parse-error)]))))

(define (factor list)
  (when (good-list? list)
    (let ([token (first list)])
      (cond [(string=? "id" token)
             (match2 "id" list)] 
            [(string=? "number" token)
             (match2 "number" list)]
            [(string=? "(" token)
             (match2 ")" (expr (match2 "(" list)))]
            [else (raise-parse-error)]))))

(define (add-op list)
  (when (good-list? list)
    (let ([token (first list)])
      (cond [(string=? "+" token)
             (match2 "+" list)]
            [(string=? "-" token)
             (match2 "-" list)]
            [else (raise-parse-error)]))))

(define (mult-op list)
  (when (good-list? list)
    (let ([token (first list)])
      (cond [(string=? "*" token)
             (match2 "*" list)]
            [(string=? "/" token)
             (match2 "/" list)]
            [else (raise-parse-error)]))))
