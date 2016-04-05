#! /usr/bin/env bash

set -e

DOCKER_ZK_CLUSTER_SIZE=${DOCKER_ZK_CLUSTER_SIZE:-3}

function run4lw() {                                                                                          
  exec 5<>/dev/tcp/$2/$3;echo $1 >&5;cat <&5 | egrep "Mode: "                            
}                                                                                          
                                                                                            
function srvr() {                                                                                          
  run4lw srvr $1 $2 2> /dev/null                                                         
}     


for index in $(seq "1" "${DOCKER_ZK_CLUSTER_SIZE}");
do
    CONTAINER_IP=$(docker inspect -f '{{.NetworkSettings.Networks.zk.IPAddress}}' zookeeper${index})
    echo "${CONTAINER_IP}:2181 $(srvr ${CONTAINER_IP} 2181)" 
done

