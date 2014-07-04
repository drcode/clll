# clll

Run Ethereum LLL contracts directly from Clojure

## Usage

With this library, you can create and run LLL code directly in Clojure. 

*Warning* This code is still pre-alpha: It can pass the cpp-ethereum POC5 example contracts, but can do little else. It is still incomplete and is missing functions. Also, it still has known incorrect behavior (for use cases beyond current unit tests)

### Differences between CLLL and LLL

The CLLL dialect is exactly the same as the LLL language, except for the following:

1. The curly braces {...} need to be replaced by (do ...) to make the Clojure reader happy.
2. The optional colon in LLL is not allowed

### Defining a Contract

To use this library, put the following dependency in your project.clj file:

```Clojure
[clll "0.1.0-SNAPSHOT"]
```

Then require the necessary libraries in your namespace declaration:

```Clojure
(ns foo.core
  (:require [clll.core :refer :all]
            [clll.functions :refer :all]))

```

Now you can write an LLL contract right in your Clojure source file:

```Clojure
(defcontract key-value-publisher
             [[69]] (caller)
             (return 0 (lll
                        (when (= (caller) @@69)
                              (for (do) (< @i (calldatasize)) [i](+ @i 64)
                                   [[ (calldataload @i) ]] (calldataload (+ @i 32))))
                        0)))
```

### Executing a Contract

The contract is just a regular clojure function with some parameters:

* The storage map
* The transaction
* An optional list of return values for external contract calls (external contract calls are only simulated at this time) 
* An optional timestamp. 

Therefore, you can just call the contract with those values: (We're starting with empty storage and a transaction that specifies the caller as "Bob")

```Clojure
> (key-value-publisher {} {:caller "Bob"})
{:storage {69 Bob}, :result #<eval$contract_eval$fun__40 clll.eval$contract_eval$fun__40@464b6>}
```

Note that this initial call is the initialization for the contract. The "real" contract has now been returned as a result, which can again be called:

```Clojure
> (let [initialized-contract     (key-value-publisher {} {:caller "Bob"})
        {:keys [result storage]} initialized-contract]
      (result storage 
              {:caller "Bob"
               :data   ["favorite-color" "green"]}))
{:storage {"favorite-color" "green", 69 Bob}, :result nil}
```

As expected, the contract has added Bob's favorite color into storage.

### Examples

The file src/clll-examples/core.clj contains all of Gavin Wood's current POC5 examples.

### Unit Tests

The file test/clll/clll-tests.clj contains comprehensive-ish test cases for Gavin's examples. You can verify that the tests pass by cloning this git repo and running "lein test" from the command line.

## License

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
