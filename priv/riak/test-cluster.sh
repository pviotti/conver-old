#! /usr/bin/env bash

set -e

RANDOM_CONTAINER_ID=$(docker ps | egrep "hectcastro/riak" | cut -d" " -f1 | shuf | head -n1)
CONTAINER_IP=$(docker inspect -f '{{.NetworkSettings.IPAddress}}' $RANDOM_CONTAINER_ID)

curl -s "http://${CONTAINER_IP}:8098/stats" -H "Accept: text/plain" | grep ring_ownership

