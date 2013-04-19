#!/bin/bash

Mbps="000000"

dpid=$1
port_num=$2
queue_id=$3
min_rate="$4$Mbps" # convert to Mbps
max_rate="$5$Mbps" # convert to Mbps

queue_name="q$queue_id"

if [ "$max_rate" == $Mbps ]; then
	echo "Error: requires <dpid> <port_num> <queue_id> <min_rate> <max_rate>"
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

if [ ! "$dec_dpid" -eq "$dpid" ]; then
    echo "Error: could not find switch name in `ovs-vsctl | list-br` !"
    exit 1
fi

switch_name=$name

##
# Convert port number to port device name

port_dev=`ovs-ofctl show $switch_name | grep addr | awk -F: '(NR=='$port_num'){ print $1 }' | cut -d\( -f2 | cut -d\) -f1`

##
# Determine which properties to set

if [ ! "$min_rate" -eq $Mbps ]; then
    min_rate_prop="other-config:min-rate=$min_rate"
fi

if [ ! "$max_rate" -eq $Mbps ]; then
    max_rate_prop="other-config:max-rate=$max_rate"
fi

##

qos_exists=`ovs-vsctl list QoS $port_dev 2>&1 | grep "no row"`
old_queue=""

if [ "$qos_exists" != "" ]; then
	ovs-vsctl -- set Port $port_dev qos=@newqos \
		-- --id=@newqos create QoS type=linux-htb other-config:max-rate=1000000000 queues=0=@default \
		-- --id=@default create Queue other-config:min-rate=1 # other-config:max-rate=500000000
else
	# Check if we are doing a modify
	old_queue=`ovs-vsctl list QoS $port_dev | grep "^queues" | awk -F{ '{ print $2 }' | cut -d} -f1 | tr ',' '\n' | grep "$queue_id=" | cut -d= -f2`
fi

if [ "$old_queue" == "" ]; then
    ovs-vsctl -- add QoS $port_dev queues $queue_id=@$queue_name \
              -- --id=@$queue_name create Queue $min_rate_prop $max_rate_prop
else
    ovs-vsctl -- set QoS $port_dev queues:$queue_id=@$queue_name \
              -- --id=@$queue_name create Queue $min_rate_prop $max_rate_prop
	ovs-vsctl destroy Queue $old_queue
fi
