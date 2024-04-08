#lang racket

(require "AccountInit.rkt")
(require "AccountTransactions.rkt")

(define (generate-statement [output-filename "STATEMENTS.TXT"])
  (let
      ([out (open-output-file output-filename #:exists 'replace)]
       [accounts (process-transactions (file->accounts) (file->transaction-lines))])
    (write "Hello World" out)
    (write "Test2" out)
    (writeln "Test3" out)
    (writeln "test4" out)
  (close-output-port out)))