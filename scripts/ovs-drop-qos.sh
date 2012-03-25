#!/bin/bash

for i in `ovs-vsctl list-ports br0`; do
	ovs-vsctl -- destroy QoS $i -- clear Port $i qos
done

for i in `ovs-vsctl list Queue | grep _uuid | awk -F: '{ print $2 }'`; do
	ovs-vsctl destroy Queue $i
done
