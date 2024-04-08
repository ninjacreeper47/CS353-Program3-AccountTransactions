#lang racket

(require "AccountInit.rkt")
(require "AccountTransactions.rkt")

(define (generate-statements [output-filename "STATEMENTS.TXT"])
  (let
      ([out (open-output-file output-filename #:exists 'replace)]
       [accounts (process-transactions (file->accounts) (file->transaction-lines))])
    (hash-for-each accounts (lambda (x acc) (print-account-statement acc out)))
  (close-output-port out)))

;Precondition: Takes in an account and an output port
;Postcondition:  Writes a statement to out
(define (print-account-statement account out)
  (statement-header account out)
  (statement-body account out)
  (statement-footer account out))

(define tab
  "    ")
(define line-seperator
  "*********************************************************")
(define (statement-header acc out)
  (displayln "STATEMENT OF ACCOUNT" out)
  (display (account-ID acc) out)
  (display tab out)
  (display (account-name acc) out)
  (display tab out)
  (display "Starting Balance: " out)
  (display tab out)
  (display (account-start-balance acc ) out)
  (displayln "" out))

(define (statement-body acc out)
  (print-transaction (account-transactions acc) out))

(define (print-transaction trans-list out)
  (if (empty? trans-list)
      (displayln "" out)
      (begin
        (displayln (transaction->string (first trans-list)) out))))
(define (statement-footer acc out)
  null)

(define (transaction->string trans)
  (string-append (number->string (transaction-ID trans))) (number->string (transaction-account-num trans)))