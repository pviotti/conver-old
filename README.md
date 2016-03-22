# Con:heavy_check_mark:er

Conver verifies implementations of the most common non-transactional consistency models.


## Features

Conver performs random reads and writes on the chosen distributed store
and records their outcomes.
Using that, Conver checks whether the execution respected
some of the most common storage consistency semantics.

Datastores supported:

 * [x] Dummy (in-memory *ets*)
 * [ ] Riak
 * [ ] ZooKeeper

Consistency semantics currently supported:

 * [x] Monotonic Reads
 * [x] Monotonic Writes
 * [x] Read-your-writes
 * [x] PRAM
 * [x] Writes-follow-reads
 * [x] Causal
 * [x] Linearizability (with chosen linearization points)

Besides a textual output, Conver generates a visualization of the executions,
highlighting possible consistency anomalies and violations of consistency models.

![Conver execution](/ex-mock.png?raw=true)


## Getting started

Requirements:

 * Erlang/OTP - tested with R18.2
 * [PropEr](http://proper.softlab.ntua.gr/)

To build:

    erl -make

To run a simple execution:

    erl -pa ./ebin -eval "cv_main:run(5, mock)." -s init stop -noshell

where 5 is the number of client processes, and "mock" is the name of the store under test
(in this case, a dummy in-memory store consisting of Erlang's *ets*).
After the execution, Conver prints the outcome to stdout, and draws the corresponding graph
to a file in the current directory named as `client_type.png`.

To run tests with PropEr:

    erl -pa ./ebin -eval "cv_statem:test()." -s init stop -noshell


## Documentation

To have an overview of the consistency models checked by Conver, see [this survey][survey].
The approach implemented in Conver has been described in [this PaPoC 2016 paper][papoc].

Related project:

 * [Jepsen][jepsen]
 * [Hermitage][hermitage]

## Authors and license

Hybris has been developed at [EURECOM][eurecom].
License: Apache 2.0.


 [survey]: http://arxiv.org/abs/1512.00168
 [papoc]: http://
 [jepsen]: http://jepsen.io/
 [hermitage]: https://github.com/ept/hermitage
 [eurecom]: http://www.eurecom.fr