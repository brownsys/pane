#!/bin/bash

DNP_SERVER=localhost
DNP_PORT=4242
WAIT_TIME=1   # seconds

nc -w $WAIT_TIME $DNP_SERVER $DNP_PORT << EOF
adf.
ListShares.
EOF
