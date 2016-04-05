#! /usr/bin/env bash

set -e

DOCKER_ZK_CLUSTER_SIZE=${DOCKER_ZK_CLUSTER_SIZE:-3}

if docker ps -a | grep "pviotti/zookeeper" >/dev/null; then
  echo -e "\nIt looks like you already have some ZooKeeper containers running."
  echo    "Please take them down before attempting to bring up another"
  echo -e "cluster with the following command:\n"
  echo -e "  make stop-cluster\n"
  exit 1
fi

echo -e "\nBringing up cluster nodes:\n"

# TODO fix cluster ip addresses (now hardcoded)

for index in $(seq "1" "${DOCKER_ZK_CLUSTER_SIZE}");
do

  if [ "${index}" -gt "1" ] ; then
	docker run -e "MYID=${index}" \
			   -e "SERVERS=172.17.0.2,172.17.0.3,172.17.0.4" \
		       --link "zookeeper1:seed" \
		       --name="zookeeper${index}" \
			   -d pviotti/zookeeper:3.4.8 > /dev/null 2>&1
  else
	docker run -e "MYID=${index}" \
			   -e "SERVERS=172.17.0.2,172.17.0.3,172.17.0.4" \
		       --name="zookeeper${index}" \
			   -d pviotti/zookeeper:3.4.8 > /dev/null 2>&1
  fi
  
  CONTAINER_ID=$(docker ps | grep "zookeeper${index}" | cut -d" " -f1)
  CONTAINER_IP=$(docker inspect -f '{{.NetworkSettings.IPAddress}}' $CONTAINER_ID)

  echo "Started zookeeper${index} (${CONTAINER_IP})"
done

