# Con:heavy_check_mark:er

Conver is a tool to verify implementations of consistency models.
It performs random read and write operations on a given distributed store and records their outcomes
in order to check the compliance with the most common consistency semantics.

![Conver execution](https://raw.github.com/pviotti/conver/master/ex-mock.png)

Datastores supported:

 * [x] Dummy (in-memory ets)
 * [ ] Riak
 * [ ] Cassandra

Consistency semantics currently supported:

 * [x] RYW
 * linearizability (w. chosen linearization points)

## Build

Requirements:

 * Erlang/OTP - tested with R18.2
 * [PropEr](http://proper.softlab.ntua.gr/)

To build:

> erl -make


## Run

> erl -pa ./ebin -eval "cv_main:run(5, mock)." -s init stop -noshell

In this example, 5 is the number of client processes, and "mock" is the name of the store under test
(in this case, a dummy in-memory store consisting of Erlang's *ets*).
After the execution, Conver prints the outcome to stdout, and draws the corresponding graph
to a file in the current directory named as `client_type.png`.  

To run tests with PropEr:

> erl -pa ./ebin -eval "cv_statem:test()." -s init stop -noshell
    

## License

Apache 2.0.
