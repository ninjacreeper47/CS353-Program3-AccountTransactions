#lang racket

(require "AccountInit.rkt")
(require "AccountTransactions.rkt")

(define (generate-statement [output-filename "STATEMENTS.TXT"])
  (let
      ([out (open-output-file output-filename #:exists 'replace)]
       [accounts (process-transactions (file->accounts) (file->transaction-lines))])
    (write accounts out)
  (close-output-port out)))