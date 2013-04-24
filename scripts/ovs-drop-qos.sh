#!/bin/bash

dpid=$1

if [ "$dpid" == "" ]; then
	echo "Error: requires <dpid>"
	exit 1
fi

##
# First, convert datapath id to switch name
#
# we can't use ovs-vsctl's find to get the switch name directly since it stores
# the DPID as a string, and we need to use numeric equality checking

for name in `ovs-vsctl list-br`; do
    tmp_dpid=`ovs-vsctl -- --columns=datapath_id find Bridge name="$name" | cut -d\" -f2`
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
# Now, delete the QoS configuration

for i in `ovs-vsctl list-ports $switch_name`; do
	ovs-vsctl -- destroy QoS $i -- clear Port $i qos
done

for i in `ovs-vsctl list Queue | grep _uuid | awk -F: '{ print $2 }'`; do
	ovs-vsctl destroy Queue $i
done
