#!/usr/bin/python
import PaneTest
from time import sleep

topo = PaneTest.TreeTopo(depth=1,fanout=2)
test = PaneTest.Test(topo)
h1 = test.net.hosts[0]
h2 = test.net.hosts[-1]
PaneTest.check(test.ping(h1, h2, 0.5, 5),
    0,
    "initial packets lost")
PaneTest.check(test.client.request("deny(srcHost=%s,dstHost=%s) on rootShare \
    from now to +5." % (h1.IP(), h2.IP())),
    True,
    "deny failed")
sleep(1)
PaneTest.check(test.ping(h1, h2, 0.5, 5), 
    100, 
    "packets should be dropped")
print "Packets successfully dropped. Sleeping until deny expires ..."
sleep(2)
PaneTest.check(test.ping(h1, h2, 0.5, 5), 
    0, 
    "deny not uninstalled")
