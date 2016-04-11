# Con:heavy_check_mark:er [![Build Status](https://travis-ci.org/pviotti/conver.svg?branch=master)](https://travis-ci.org/pviotti/conver)

Conver verifies implementations of the most common non-transactional consistency models.  

Conver spawns client processes that perform concurrent reads
and writes on the distributed store, and records their outcomes.
Then it builds graph entities that express ordering and mutual visibility of operations.
Finally, it uses such graph entities to check consistency semantics
defined as first-order logic predicates.  

The approach implemented in Conver has been described in [this PaPoC 2016 paper][papoc].  

## Features

Datastores currently supported: **ZooKeeper** and **Riak**.  

Currently, Conver can verify the following consistency semantics: Monotonic Reads, Monotonic Writes,
Read-your-writes, PRAM, Writes-follow-reads, Causal and Strong Consistency (regularity).  
To have an overview of the consistency models verified by Conver, see [this survey][survey].  

Besides textual output, Conver generates a visualization of the executions,
highlighting the violations of consistency models.  

![Conver execution](http://i.imgur.com/BDGtpzX.png)

## Getting started

Once installed Erlang/OTP (R18+), to build Conver issue:

    $ make

The `dstores` folder contains scripts to build and locally run 
[Docker](https://www.docker.com/) clusters of different storage systems.  
For instance, to build and run a cluster of ZooKeeper servers
issue `make build-zk` and `make start-zk`.  
To query the cluster status use `make status-zk`, and to stop it `make stop-zk`.  
Additionally, the script `dstores/net.sh` offers functions to simulate
slow and lossy networks.  

Once the storage system is running and its configuration parameters are
recorded in `conver.conf`, to run a Conver test issue:

    $ ./conver run <num> <mock|zk|riak>

where `num` is the number of client processes (e.g., 3),
followed by a string that identifies the store under test
(`mock` for a dummy in-memory store consisting of Erlang's *ets*,
`zk` for ZooKeeper, `riak` for Riak).  

For comparison, you can run a *traditional* property-based test suite
on the dummy in-memory database using 
[PropEr](http://proper.softlab.ntua.gr/) by issuing `./conver proper`.  

Similar projects: [Jepsen][jepsen], [Hermitage][hermitage].  

## Authors and license

Conver has been developed at [EURECOM][eurecom].  
License: Apache 2.0.


 [survey]: http://arxiv.org/abs/1512.00168
 [papoc]: http://www.eurecom.fr/fr/publication/4874/download/ds-publi-4874.pdf
 [jepsen]: http://jepsen.io
 [hermitage]: https://github.com/ept/hermitage
 [eurecom]: http://www.eurecom.fr
