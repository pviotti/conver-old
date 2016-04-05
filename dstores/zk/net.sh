#!/bin/bash


function get_container_interface() {
  if_num=$(( $(docker exec $1 ip link show eth0 | head -1 | cut -d":" -f1) +1 ))
  echo $(ip link | grep $if_num | cut -d":" -f2 | cut -d"@" -f1 | tr -d '[[:space:]]')
}

function apply_delay() {
    sudo tc qdisc replace dev $1 root netem delay 75ms 100ms distribution normal
}

function apply_lossy() {
    sudo tc qdisc replace dev $1 root netem loss 30%
}

function remove_qdisc_filters() {
    sudo tc qdisc del dev $1 root
}

function show_qdisc() {
    sudo tc qdisc show dev $1
}

DOCKER_ZK_CLUSTER_SIZE=${DOCKER_ZK_CLUSTER_SIZE:-3}

case "$1" in
slow)
    for index in $(seq "1" "${DOCKER_ZK_CLUSTER_SIZE}");
    do
        container_if=$(get_container_interface $(docker ps -aqf "name=zookeeper$index"))
        apply_delay $container_if
    done
    ;;
lossy)
    for index in $(seq "1" "${DOCKER_ZK_CLUSTER_SIZE}");
    do
        container_if=$(get_container_interface $(docker ps -aqf "name=zookeeper$index"))
        apply_lossy $container_if
    done
    ;;
normal)
    for index in $(seq "1" "${DOCKER_ZK_CLUSTER_SIZE}");
    do
        container_if=$(get_container_interface $(docker ps -aqf "name=zookeeper$index"))
        remove_qdisc_filters $container_if
    done;
    ;;
show)
    for index in $(seq "1" "${DOCKER_ZK_CLUSTER_SIZE}");
    do
        container_if=$(get_container_interface $(docker ps -aqf "name=zookeeper$index"))
        echo -n "$container_if: " 
        show_qdisc $container_if
    done;
    ;;
*)
  echo $"Usage: $0 {slow|lossy|normal|show}"
  exit 1
esac

