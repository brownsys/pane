#!/usr/bin/python
import PaneTest

topo = PaneTest.TreeTopo(depth=2,fanout=2)
test = PaneTest.Test(topo)
for src in test.net.hosts:
  for dst in test.net.hosts:
    if src == dst:
      continue
    PaneTest.check(test.ping(src, dst, 1, 3), 0, "packets lost")
