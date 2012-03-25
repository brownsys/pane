#!/bin/bash

port_num=$1
queue_id=$2
min_rate="$3000000" # convert to Mbps

queue_name="q$queue_id"

if [ "$min_rate" == "" ]; then
	echo "Error: requires <port_num> <queue_id> <min_rate>"
	exit
fi

port_dev=`ovs-ofctl show br0 | grep addr | awk -F: '(NR=='$port_num'){ print $1 }' | cut -d\( -f2 | cut -d\) -f1`

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
        	-- --id=@$queue_name create Queue other-config:min-rate=$min_rate
else
        ovs-vsctl -- set QoS $port_dev queues:$queue_id=@$queue_name \
                -- --id=@$queue_name create Queue other-config:min-rate=$min_rate
	ovs-vsctl destroy Queue $old_queue
fi
