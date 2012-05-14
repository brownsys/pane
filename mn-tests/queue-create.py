#!/usr/bin/python
import PaneTest
from time import sleep

topo = PaneTest.TreeTopo(depth=1,fanout=2)
test = PaneTest.Test(topo)
h1 = test.net.hosts[0]
h2 = test.net.hosts[-1]
PaneTest.check(test.ping(h1, h2, 0.5, 2),
    0,
    "initial packets lost")
PaneTest.check(test.client.request("NewShare net0 for (*) [reserve <= 200] on \
    rootShare."),
    True,
    "should be able to create a new share")
PaneTest.check(test.client.request("deny (srcHost=%s,dstHost=%s) on \
    rootShare." % (h1.IP(), h2.IP())),
    True,
    "deny should succeed")
sleep(1)
PaneTest.check(test.ping(h1, h2, 0.5, 2),
    100,
    "packets should be lost")

PaneTest.check(test.client.request("reserve (srcHost=%s,dstHost=%s) = 20 on \
    net0." % (h1.IP(), h2.IP())),
    True,
    "reservation should succeed")
sleep(1)
print "Pinging ..."
PaneTest.check(test.ping(h1, h2, 0.5, 2), 
    0, 
    "packets should go through after reservation")
print "Test successful."
