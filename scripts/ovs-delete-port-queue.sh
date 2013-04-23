#!/bin/bash

dpid=$1
port_num=$2
queue_id=$3

if [ "$queue_id" == "" ]; then
	echo "Error: requires <dpid> <port_num> <queue_id>"
	exit 1
fi

if [ "$queue_id" == "0" ]; then
	echo "Will not delete default queue"
	exit 1
fi

##
# First, convert datapath id to switch name

for name in `ovs-vsctl list-br`; do
    tmp_dpid=`ovs-ofctl show $name | grep dpid | awk -F"dpid:" '{ print $2 }'`
    dec_dpid=$((0x$tmp_dpid))
    if [ "$dec_dpid" -eq "$dpid" ]; then
        break
    fi
done

if [ "$dec_dpid" == "" ] || [ ! "$dec_dpid" -eq "$dpid" ]; then
    echo "Error: could not find switch name in '`ovs-vsctl list-br`'!"
    exit 1
fi

switch_name=$name

##
# Convert port number to port device name, and find queue UUID

port_dev=`ovs-ofctl show $switch_name | grep addr | awk -F: '(NR=='$port_num'){ print $1 }' | cut -d\( -f2 | cut -d\) -f1`

queue_uuid=`ovs-vsctl get QoS $port_dev queues:$queue_id 2>/dev/null`

if [ "$queue_uuid" == "" ]; then
	echo "Port $port_num, Queue $queue_id does not exist"
	exit 1
fi

ovs-vsctl -- remove QoS $port_dev queues $queue_id=$queue_uuid \
          -- destroy Queue $queue_uuid

# if record only contains default queue, could do...
# ovs-vsctl -- destroy QoS $port_dev -- clear Port $port_dev qos <-- must be done together
