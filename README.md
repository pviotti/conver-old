# Consistency tester

A tool to perform property-based testing of consistency semantics.

## Build

Requirements:

 * Erlang/OTP (tested with R18.2)
 * [PropEr](http://proper.softlab.ntua.gr/)

To build:

    erl -make

## Run


    erl -pa ./ebin -eval "ct_orchestrator:run(5, \"mock\")." -s init stop -noshell

where 5 is the number of clients and "mock" is the client type
(in this case a client testing against a dummy in-memory store consisting of Erlang's ets).  
It should print a trace of the execution and then draw the corresponding graph in the current 
directory as ``client_type.png``.  
  
  
To run tests with PropEr:

    erl -pa ./ebin -eval "ct_statem:test()." -s init stop -noshell
    
   
    
## License

TODO
