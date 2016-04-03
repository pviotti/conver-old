#! /usr/bin/env bash

set -e

docker ps | egrep "hectcastro/riak" | cut -d" " -f1 | xargs -I{} docker rm -f {} > /dev/null 2>&1

echo "Stopped the cluster and cleared all of the running containers."
