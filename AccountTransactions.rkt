#lang racket

(require "AccountInit.rkt")

(struct transaction (transaction-ID account-num purchase/payment timestamp))
(struct purchase (merchant amount))