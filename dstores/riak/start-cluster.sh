#! /usr/bin/env bash

set -e

DOCKER_RIAK_CLUSTER_SIZE=${DOCKER_RIAK_CLUSTER_SIZE:-3}
DOCKER_RIAK_BACKEND=${DOCKER_RIAK_BACKEND:-bitcask}

if docker ps -a | grep "hectcastro/riak" >/dev/null; then
  echo -e "\nIt looks like you already have some Riak containers running."
  echo    "Please take them down before attempting to bring up another"
  echo -e "cluster with the following command:\n"
  echo -e "  make stop-cluster\n"
  exit 1
fi

echo -e "\nBringing up cluster nodes:\n"

for index in $(seq "1" "${DOCKER_RIAK_CLUSTER_SIZE}");
do

  if [ "${index}" -gt "1" ] ; then
    docker run -e "DOCKER_RIAK_CLUSTER_SIZE=${DOCKER_RIAK_CLUSTER_SIZE}" \
               -e "DOCKER_RIAK_AUTOMATIC_CLUSTERING=1" \
               -e "DOCKER_RIAK_BACKEND=${DOCKER_RIAK_BACKEND}" \
               --link "riak1:seed" \
               --name "riak${index}" \
               -d hectcastro/riak > /dev/null 2>&1
  else
    docker run -e "DOCKER_RIAK_CLUSTER_SIZE=${DOCKER_RIAK_CLUSTER_SIZE}" \
               -e "DOCKER_RIAK_AUTOMATIC_CLUSTERING=1" \
               -e "DOCKER_RIAK_BACKEND=${DOCKER_RIAK_BACKEND}" \
               --name "riak${index}" \
               -d hectcastro/riak > /dev/null 2>&1
  fi
  echo -n "Starting riak${index}: "

  CONTAINER_ID=$(docker ps | grep "riak${index}" | cut -d" " -f1)
  CONTAINER_IP=$(docker inspect -f '{{.NetworkSettings.IPAddress}}' $CONTAINER_ID)

  until curl -m 1 -s "http://${CONTAINER_IP}:8098/ping" | grep "OK" > /dev/null 2>&1;
  do
    echo -n "."
    sleep 1
  done

  echo " Complete."
done

echo -e "\nPlease wait approximately 30 seconds for the cluster to stabilize.\n"
