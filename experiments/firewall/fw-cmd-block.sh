#!/bin/bash

DNP_SERVER=localhost
DNP_PORT=4242
WAIT_TIME=1   # seconds

DST_HOST="10_0_0_100"
SRC_HOST=`echo $1 | tr '.' '_'`    # TODO: temp hack to deal with identifiers not having .'s
AC_SHARE="adfHostAccessControl"

nc -w $WAIT_TIME $DNP_SERVER $DNP_PORT << EOF
adf.
deny(dstHost=$DST_HOST, srcHost=$SRC_HOST) on $AC_SHARE.
EOF
