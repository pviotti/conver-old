.PHONY: build start test stop 

build:
	docker pull "pviotti/zookeeper"

start:
	./start-cluster.sh

status:
	./test-cluster.sh

stop:
	./stop-cluster.sh
