#!/bin/bash

##
# Delete ALL QoS configuration

for i in `ovs-vsctl list QoS | grep _uuid | awk -F: '{ print $2 }'`; do
	ovs-vsctl -- destroy QoS $i
done

for i in `ovs-vsctl list Queue | grep _uuid | awk -F: '{ print $2 }'`; do
	ovs-vsctl destroy Queue $i
done
