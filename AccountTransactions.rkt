#lang racket

(require "AccountCode.rkt")

(struct transaction (transaction-ID account-num purchase/payment timestamp))
(struct purchase (merchant amount))