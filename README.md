# Con:heavy_check_mark:er

Conver verifies implementations of the most common non-transactional consistency models.


## Features

Conver spawns client processes that perform concurrent reads 
and writes on the distributed store, and records their outcomes.  
Then it builds graph entities that express ordering and mutual visibility of operations.  
Finally, Conver uses these graph entities to check consistency semantics
defined as first-order logic predicates.  

The approach implemented in Conver has been described in [this PaPoC 2016 paper][papoc].  
To have an overview of the consistency models verified by Conver, see [this survey][survey].  
Similar projects: [Jepsen][jepsen], [Hermitage][hermitage].  

Datastores supported:

 * [x] Dummy (in-memory *ets*)
 * [x] ZooKeeper
 * [x] Riak

Currently, Conver can verify the following consistency models: Monotonic Reads, Monotonic Writes,
Read-your-writes, PRAM, Writes-follow-reads, Causal and Linearizability (with chosen linearization points).  

Besides textual output, Conver generates a visualization of the executions,
highlighting the violations of consistency models.

![Conver execution](/ex-mock.png?raw=true)


## Getting started

Once installed Erlang/OTP (R18+), to build Conver issue:

    $ make

To make Conver test a simple execution:

    $ ./conver run <num> <mock|zk|riak>

where `num` is the number of client processes (e.g., 3),
followed by a string that identifies the store under test
(`mock` for a dummy in-memory store consisting of Erlang's *ets*,
`zk` for ZooKeeper, `riak` for Riak).  

To run a demonstrative test of the dummy datastore using PropEr:

    $ ./conver proper


## Authors and license

Conver has been developed at [EURECOM][eurecom].  
License: Apache 2.0.


 [survey]: http://arxiv.org/abs/1512.00168
 [papoc]: http://
 [jepsen]: http://jepsen.io/
 [hermitage]: https://github.com/ept/hermitage
 [eurecom]: http://www.eurecom.fr
