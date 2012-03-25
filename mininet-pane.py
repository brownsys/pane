#!/usr/bin/python
from subprocess import Popen
from mininet.node import RemoteController
from mininet.net import Mininet
from mininet.topolib import TreeTopo
from mininet.topo import LinearTopo
import re

controller = Popen(['./dnp', '-n', '4242'])
try:
  theTopo = TreeTopo(depth=2,fanout=2)
  #theTopo = LinearTopo(k=3)
  net = Mininet(topo=theTopo,controller=RemoteController)
  net.start()
  print "Starting ping storm ..."
  for src in net.hosts:
    for dst in net.hosts:
      if src == dst:
        continue
      cmd = 'ping -i 0.2 -c5 %s' % dst.IP()
      out = src.cmd(cmd)
      m = re.search(r"(\d+)% packet loss", out)
      if True:
#      if m.group(1) != "0":
        print '%s$ %s' % (src.IP(), cmd)
        print out
  net.stop()
finally:
  controller.kill()
