#!/bin/bash

port_num=$1
queue_id=$2

if [ "$queue_id" == "" ]; then
	echo "Error: requires <port_num> <queue_id>"
	exit
fi

if [ "$queue_id" == "0" ]; then
	echo "Will not delete default queue"
	exit
fi

port_dev=`ovs-ofctl show br0 | grep addr | awk -F: '(NR=='$port_num'){ print $1 }' | cut -d\( -f2 | cut -d\) -f1`

queue_uuid=`ovs-vsctl list QoS $port_dev | grep "^queues" | awk -F{ '{ print $2 }' | cut -d} -f1 | tr ',' '\n' | grep "$queue_id=" | cut -d= -f2`

if [ "$queue_uuid" == "" ]; then
	echo "Port $port_num, Queue $queue_id does not exist"
	exit
fi

ovs-vsctl -- remove QoS $port_dev queues $queue_id=$queue_uuid

ovs-vsctl destroy Queue $queue_uuid

# if record only contains default queue, could do...
# ovs-vsctl -- destroy QoS $port_dev -- clear Port $port_dev qos <-- must be done together
