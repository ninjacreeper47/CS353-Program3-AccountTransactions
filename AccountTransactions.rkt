#lang racket

;This module is dedicated to processing transactions
(require "AccountInit.rkt")

(provide (struct-out transaction))
(provide (struct-out purchase))
(provide (struct-out payment))
(provide file->transaction-lines)
(provide process-transactions)

(struct transaction (ID account-num timestamp purchase/payment ) #:inspector #f)
(struct purchase (merchant amount))
(struct payment (pay-method amount)) ; a pay-method is a list containing a label then method-specific data 




;Precondition: Takes in a string representing an input file name. Lines on this file match the formating expected of Transaction Input
;Postcondition: Returns a list of strings where each string is a line of filename
(define (file->transaction-lines [filename "TRANSACTIONS.TXT"])
   (file->lines filename))

;Precondition: Takes in a hash of accounts and  a list of strings formatted as transactions, and optionally a starting number for ID assignment
;Postcondition: Returns a new version of accounts, updated with the transactions from trans-list

;This function calls itself recurivesely in order to give each transaction an ascending ID
;The only mechanism for ensuring ID uniqueness is the starting value for trans ID.
;If this function is called more than once externally,  it is the caller's responsibility to provide a valid starting value for trans-ID (higher than any IDs in other transactions)
(define (process-transactions accounts trans-list [trans-ID 10001])
  (if (empty? trans-list)
      accounts
      (process-transactions (find-account-and-add-transaction accounts (first trans-list) trans-ID) (rest trans-list) (+ trans-ID 1)))) 

;Precondition: Takes in a hash of accounts,  a single string formated to represent  a transaction, and a numerical ID to assign to that transaction
;Postcondition:  Returns a new version of  accounts, where the account matching the customer ID in trans-line is updated with the additionial transaction
(define (find-account-and-add-transaction accounts trans-line trans-ID)
  (let ([port:line (open-input-string trans-line)])
    (let ([trans-type (read port:line)] [customer-num (read port:line)] [time-stamp (read port:line)])
      (hash-update accounts customer-num
                   (lambda (acc) (new-transaction-on-account
                                  acc
                                  (transaction trans-ID customer-num time-stamp (gather-type-specific-information trans-type port:line))))))))

;Precondition:  Takes in a trans-type string, which must either be Purchase or Payment.
;Takes in a port where the stream begins with the relevant entries for the type

;Postcondition: Returns either a Payment or a Purchase
;If trans-type is not Purchase or Payment, returns an error
;SIDE-EFFECT: Consumes 0-3 data-reads from port (amount depends on trans-type and payment method) 
(define (gather-type-specific-information trans-type port)
  (cond
    [(equal? trans-type 'Purchase) (build-purchase port)]
    [(equal? trans-type 'Payment) (build-payment port)]
    [else (error "Invalid Transaction Type")]))

;Precondition: Takes in a port where the first entry is merchant name and the second is an amount
;Postcondition: Returns a Purchase
;SIDE-EFFECT: Consumes 2 data-reads from port
(define (build-purchase port)
  (purchase (read port) (read port)))

;Precondition: Takes in a port where the first entry is a payment type, and subsuquent entries correspond to the payment information and then an amount
;Postcondition: Returns a Payment
;SIDE-EFFECT: Consumes 2-3 data reads from port
(define (build-payment port)
  (let ([pay-type (read port)])
    (payment
     (cond
      [(equal? pay-type 'Credit) (list "Credit" (read port))]
      [(equal? pay-type 'Debit) (list "Debit" (read port))]
      [(equal? pay-type 'Check) (list "Check" (read port))]
      [(equal? pay-type 'Cash) (list "Cash")])
     (read port))))



;Precondition: Takes in an account and a transaction
;Postcondition: returns that account with the transaction appeneded to its transaction list
(define (new-transaction-on-account target-account transaction)
  (account
   (account-ID target-account)
   (account-name target-account)
   (account-start-balance target-account)
   (append  (account-transactions target-account)(list transaction)))) 
  
    
