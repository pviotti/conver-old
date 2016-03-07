# Conver

Conver is a tool to verify implementations of consistency models.


## Build

Requirements:

 * Erlang/OTP - tested with R18.2
 * [PropEr](http://proper.softlab.ntua.gr/)

To build:

> erl -make


## Run

> erl -pa ./ebin -eval "cv_main:run(5, mock)." -s init stop -noshell

where 5 is the number of client processes to spin and "mock" is the store type
(in this case, a client testing against a dummy in-memory store consisting of Erlang's *ets*).
It should print a trace of the execution and then draw the corresponding graph in the current 
directory as `client_type.png`.
  
  
To run tests with PropEr:

> erl -pa ./ebin -eval "cv_statem:test()." -s init stop -noshell
    

## License

Apache 2.0.
