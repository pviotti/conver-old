.PHONY: build start test stop net-slow net-lossy net-normal net-show

build:
	docker pull "hectcastro/riak"

start:
	./start-cluster.sh

test:
	./test-cluster.sh

stop:
	./stop-cluster.sh
	
net-slow:
	./net.sh slow

net-lossy:
	./net.sh lossy

net-normal:
	./net.sh normal

net-show:
	./net.sh show
