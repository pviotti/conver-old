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

docker network create zk >/dev/null 2>&1

for index in $(seq "1" "${DOCKER_ZK_CLUSTER_SIZE}");
do
	docker run -e "MYID=${index}" \
			   -e "SERVERS=zookeeper1,zookeeper2,zookeeper3" \
               -h "zookeeper${index}" \
               --net="zk" \
		       --name="zookeeper${index}" \
			   -d pviotti/zookeeper:3.4.8 > /dev/null 2>&1
  
  CONTAINER_IP=$(docker inspect -f '{{.NetworkSettings.Networks.zk.IPAddress}}' "zookeeper${index}")
  echo "Started zookeeper${index} (${CONTAINER_IP})"
done

