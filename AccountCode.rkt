#lang racket

(provide file->accounts)
(provide (struct-out account))

(struct account (ID name balance transactions) #:inspector #f)


;Precondition:  Takes in a string representing a filename
;Postcondition: Reads the contents of the  file and returns a hash containing accounts populated from the lines of the file
(define (file->accounts [filename "ACCOUNTS.TXT"])
  (list->hash
   (map account-input-line->account
    (file->lines filename)) key-rule:account-ID))



;Precondition:  Takes in a string that contains  id, then name, then balance- all seperated by whitespace
;Postcondition: returns an account populated with information contained in the string (and an empty transaction list)
(define (account-input-line->account line)
  (let ([port:line (open-input-string line)])
    (account
     (read port:line) ;id
     (read port:line) ;name
     (read port:line) ;balance
     (list) ;transactions [starts empty]
     )))

;Precondition: takes in a list and a key-rule.  A key rule is a function that contains the logic for determining how to extract a key out of data
;Postcondition: Returns a hash where the values are the entries of the list and the keys are determined for each list entry according to key-rule

;This function calls itself recursively in order to functionially build the hash-table.  The out-hash parameter is not required for external calls
(define (list->hash in-list key-rule [out-hash (hash)])
  (if (empty? in-list)
      out-hash
      (list->hash (rest in-list) key-rule  (hash-set out-hash (key-rule (first in-list)) (first in-list)))))

;This function can be passed in as a key-rule for list->hash
;Precondition: Takes in an account
;Postcondition: Returns the ID of that account
(define (key-rule:account-ID in-account)
  (account-ID in-account))

