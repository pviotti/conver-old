#! /usr/bin/env bash

set -e

RANDOM_CONTAINER_ID=$(docker ps | egrep "pviotti/zookeeper" | cut -d" " -f1 | shuf | head -n1)
CONTAINER_IP=$(docker inspect -f '{{.NetworkSettings.IPAddress}}' $RANDOM_CONTAINER_ID)


function run4lw()                                                                          
{                                                                                          
  exec 5<>/dev/tcp/$2/$3;echo $1 >&5;cat <&5 | egrep "Mode: "                            
}                                                                                          
                                                                                            
function srvr()                                                                            
{                                                                                          
  run4lw srvr $1 $2 2> /dev/null                                                         
}     


echo "172.17.0.2:2181 $(srvr 172.17.0.2 2181)" 
echo "172.17.0.3:2181 $(srvr 172.17.0.3 2181)" 
echo "172.17.0.4:2181 $(srvr 172.17.0.4 2181)" 

