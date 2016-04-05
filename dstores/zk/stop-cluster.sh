#! /usr/bin/env bash

set -e

docker ps -a | egrep "pviotti/zookeeper" | cut -d" " -f1 | xargs -I{} docker rm -f {} > /dev/null 2>&1

docker network rm zk

echo "Stopped the cluster and cleared all of the running containers."
