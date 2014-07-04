(ns clll-examples.core
  (:require [clll.core :refer :all]
            [clll.functions :refer :all]))

;;These contracts taken from https://github.com/ethereum/cpp-ethereum/wiki/LLL-Examples-for-PoC-5
;; Only changes are:
;;    1. "{...}" has been replaced by "(do ...)"
;;    2. contract calls use literal function name, not address
;;    3. Optional colon for assignments removed

(defcontract key-value-publisher
             [[69]] (caller)
             (return 0 (lll
                        (when (= (caller) @@69)
                              (for (do) (< @i (calldatasize)) [i](+ @i 64)
                                   [[ (calldataload @i) ]] (calldataload (+ @i 32))))
                        0)))

(defcontract name-registrar
             (do
              [[(address)]] "NameReg"
              [["NameReg"]] (address)
              [[69]] (caller)
              (return 0 (lll
                         ;; If there's at least one argument
                         (if (calldatasize)
                             (do
                              ;; Stop if the first arg (name) has already been registered.
                              (when @@(calldataload 0) (stop))

                              ;; Store sender at name, and name at sender.
                              (when @@(caller) [[@@(caller)]] 0)
                              [[(calldataload 0)]] (caller)
                              [[(caller)]] (calldataload 0)
                              (stop)
                              )

                             ;; No arguments - either deregister or suicide (if it's from owner's address).
                             (do

                              ;; Suicide if it's from owner's address.
                              (when (= (caller) @@69) (suicide (caller)))

                              ;; Otherwise, just deregister any name sender has, if they are registered.
                              (when @@(caller) (do
                                                [[@@(caller)]] 0
                                                [[(caller)]] 0
                                                ))
                              (stop)
                              )
                             )
                         0))
              ))

(defcontract bank
             (do
              [0] "Bank"
              (call (- (gas) 100) "name-registrar" 0 0 4 0 0)

              (return 0 (lll (do
                              (if (>= @@(caller) (calldataload 0))
                                  ;; Withdrawal:
                                  (do
                                   ;; Subtract the value from the balance of the account
                                   [[ (caller) ]] (- @@(caller) (calldataload 0))

                                   ;; Transfer the funds either to...
                                   (if (<= (calldatasize) 32)
                                       (call (- (gas) 100) (caller) (calldataload 0) 0 0 0 0) ; ...the sender...
                                       (call (- (gas) 100) (calldataload 32) (calldataload 0) 0 0 0 0) ; ...or the supplied account.
                                       )
                                   )
                                  ;; Deposit; just increase the account balance by that amount.
                                  [[(caller)]] (+ @@(caller) (callvalue))
                                  )
                              ) 0))
              ))

(defcontract splitter
             (do
              [0] "Splitter"
              (call (- (gas) 100) "name-registrar" 0 0 8 0 0)

              (return 0 (lll (do
                              [count] (/ (calldatasize) 32)
                              [pay] (/ (callvalue) @count)

                              ;; Cycle through each address
                              (for (do) (< @i @count) [i](+ @i 1)
                                   ;; Send to 'i'th argument (assuming it's an address).
                                   (call (- (gas) 100) (calldataload (* @i 32)) @pay 0 0 0 0))) 
                             0))))
(defcontract gav-coin
             (do
              ;; Give caller a whole bunch of cash.
              [[ (caller)]] 0x1000000000000000000000000
              ;; Register with the NameReg contract.
              [0] "GavCoin"
              (call (- (gas) 100) "name-registrar" 0 0 7 0 0)

              (return 0 (lll (do
                              (when (!= (calldatasize) 64) (stop))      ; stop if there's not enough data passed.
                              [fromBal] @@(caller)
                              [toBal] @@(calldataload 0)
                              [value] (calldataload 32)
                              (when (< @fromBal @value) (stop))         ; stop if there's not enough for the transfer.
                              [[ (caller)]] (- @fromBal @value)       ; subtract amount from caller's account.
                              [[ (calldataload 0)]] (+ @toBal @value) ; add amount on to recipient's account.
                              ) 0))
              ))

(defcontract time-vault
    ;; Register with the NameReg contract.
    [0] "TimeVault"
    (call (- (gas) 100) "name-registrar" 0 0 9 0 0)

    ;; Register the owner of the money in the contract
    [[owner]] (caller)
    ;; Initialize the waiting period to 3600 seconds (60 minutes)
    [[waiting_period]] 3600

    (return 0 (lll
               ;; Owner starts a withdrawal authorisation
               (if (and (= (caller) @@owner) (= (calldataload 0) "withdrawal"))
                   [[withdrawal_start]] (timestamp)
                   ;; Else, when owner finalises withdrawal after waiting period and time has elapsed
                   (when (and (= (caller) @@owner) (= (calldataload 0) "finalize")
                              (< (+ @@withdrawal_start @@waiting_period) (timestamp)))
                   ;; Send money to sender
                         (suicide @@owner)))
               0)))
