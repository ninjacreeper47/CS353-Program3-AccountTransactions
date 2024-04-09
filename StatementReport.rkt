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
(define block-seperator
  "*********************************************************")
(define (statement-header acc out)
  (displayln "STATEMENT OF ACCOUNT" out)
  (display (account-ID acc) out)
  (display tab out)
  (display (account-name acc) out)
  (display tab out)
  (display "Starting Balance: " out)
  (display tab out)
  (displayln (account-start-balance acc ) out)
  (displayln "" out))

(define (statement-body acc out)
  (print-transaction (account-transactions acc) out)
  (displayln "" out))

(define (print-transaction trans-list out)
  (if (empty? trans-list)
      (displayln "" out)
      (begin
        (displayln (transaction->string (first trans-list)) out)
        (print-transaction (rest trans-list) out))))

(define (statement-footer acc out)
  (display (string-append "Total Purchases:" tab tab) out )
  (displayln (~r(total-purchases (account-transactions acc)) #:precision '(= 2)) out)
  (display (string-append "Total Payments:" tab tab) out)
  (displayln (~r (total-payments (account-transactions acc)) #:precision '(= 2)) out)
  (display (string-append "Ending Balance:" tab tab) out)
  (displayln (~r (ending-balance acc) #:precision '(= 2)) out)
  (displayln block-seperator out ))

(define (total-purchases trans-list [val 0])
  (if (empty? trans-list)
      val
      (let* ([trans (first trans-list)])
        (if (purchase? (transaction-purchase/payment trans))
            (total-purchases (rest trans-list) (+ val (purchase-amount (transaction-purchase/payment trans))))
            (total-purchases (rest trans-list) val)))))
        
(define (total-payments trans-list [val 0])
  (if (empty? trans-list)
      val
      (let* ([trans (first trans-list)])
        (if (payment? (transaction-purchase/payment trans))
            (total-payments (rest trans-list) (+ val (payment-amount (transaction-purchase/payment trans))))
            (total-payments (rest trans-list) val)))))
(define (ending-balance acc)
  (- (+ (account-start-balance acc) (total-purchases (account-transactions acc))) (total-payments (account-transactions acc))))

(define (transaction->string trans)
  (string-append (number->string (transaction-ID trans)) tab 
  (let ([details (transaction-purchase/payment trans)])
  (cond
    [(purchase? details)
     (string-append "Purchase" tab (purchase-merchant details) tab  (number->string (purchase-amount details)))]
    [(payment? details)
     (string-append "Payment" tab (first (payment-pay-method details)) tab (number->string (payment-amount details)))]
    [else ""]))))
