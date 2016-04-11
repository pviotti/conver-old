.PHONY: all compile clean dialyzer lint test start-zk start-riak \
  stop-zk stop-riak stop-all status-zk status-riak

REBAR = $(shell pwd)/rebar3

all: compile	

compile:
	$(REBAR) compile
	
clean:
	$(REBAR) clean
	rm -rf ./logs

dialyzer:
	$(REBAR) dialyzer

lint:
	$(REBAR) as lint lint

test:
	$(REBAR) as test eunit

# Data store targets

build-zk:
	$(MAKE) -C dstores/zk build
build-riak:
	$(MAKE) -C dstores/riak build

start-zk:
	$(MAKE) -C dstores/zk start
	./dstores/net.sh slow zookeeper
start-riak:
	$(MAKE) -C dstores/riak start
	$(MAKE) -C dstores/riak status

slow-zk:
	./dstores/net.sh slow zk
slow-riak:
	./dstores/net.sh slow riak

stop-zk:
	$(MAKE) -C dstores/zk stop
stop-riak:
	$(MAKE) -C dstores/riak stop
stop-all: stop-zk stop-riak

status-zk:
	$(MAKE) -C dstores/zk status
status-riak:
	$(MAKE) -C dstores/riak status
