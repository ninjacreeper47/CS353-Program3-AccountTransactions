#lang racket

;This is the main driver of the program and contains the logic for generating statements


(require "AccountInit.rkt")
(require "AccountTransactions.rkt")

;The main function of the program. Run this!
;Creates an output file in a statement format using  the accounts and transaction files for this assignment
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

;Precondition: Takes in an account and an output port
;Postcondition: Writes a header (statement label, ID name, start balance)  to out
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

;Precondition: takes in an account and an output port
;Postcondition: Prints each transaction in the account to out
(define (statement-body acc out)
  (print-transaction (account-transactions acc) out)
  (displayln "" out))

;Precondition: takes in a list of transactions and an output port
;Postcondition: print eaach transaction in the list to out
(define (print-transaction trans-list out)
  (if (empty? trans-list)
      (displayln "" out)
      (begin
        (displayln (transaction->string (first trans-list)) out)
        (print-transaction (rest trans-list) out))))

;Precondition: takes in an account and an output port
;Postcondition:  Prints the statment footer (purchase total, payment total, ending balance, statment seperator)  to out
(define (statement-footer acc out)
  (display (string-append "Total Purchases:" tab tab) out )
  (displayln (~r(total-purchases (account-transactions acc)) #:precision '(= 2)) out)
  (display (string-append "Total Payments:" tab tab) out)
  (displayln (~r (total-payments (account-transactions acc)) #:precision '(= 2)) out)
  (display (string-append "Ending Balance:" tab tab) out)
  (displayln (~r (ending-balance acc) #:precision '(= 2)) out)
  (displayln block-seperator out ))

;Precondition: Takes in a list of transactions
;Postcondition: Calculates the total amount among  all purchases in the transaction list

;This function calls itself recursively to calculate its total.  External calls are not intended to pass in a value parameter
(define (total-purchases trans-list [val 0])
  (if (empty? trans-list)
      val
      (let* ([trans (first trans-list)])
        (if (purchase? (transaction-purchase/payment trans))
            (total-purchases (rest trans-list) (+ val (purchase-amount (transaction-purchase/payment trans))))
            (total-purchases (rest trans-list) val)))))

;Precondition: Takes in a list of transactions
;Postcondition: Calculates the total amount among  all purchases in the transaction list

;This function calls itself recursively to calculate its total.  External calls are not intended to pass in a value parameter
(define (total-payments trans-list [val 0])
  (if (empty? trans-list)
      val
      (let* ([trans (first trans-list)])
        (if (payment? (transaction-purchase/payment trans))
            (total-payments (rest trans-list) (+ val (payment-amount (transaction-purchase/payment trans))))
            (total-payments (rest trans-list) val)))))

;Preconditoin: takes in an account
;Postcondition: Returns account starting balance + balance accumulated from purchases - balance removed by payments
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
