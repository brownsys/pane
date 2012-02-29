#!/usr/bin/python
from subprocess import Popen
from mininet.node import RemoteController
from mininet.net import Mininet
from mininet.topolib import TreeTopo
import re

controller = Popen(['./dnp', '-s', '4242'])
try:
  tree4 = TreeTopo(depth=1,fanout=2)
  net = Mininet(topo=tree4,controller=RemoteController)
  net.start()
  print "Starting ping storm ..."
  for src in net.hosts:
    for dst in net.hosts:
      cmd = 'ping -c1 %s' % dst.IP()
      out = src.cmd(cmd)
      m = re.search(r"(\d+)% packet loss", out)
      if True:
#      if m.group(1) != "0":
        print '%s$ %s' % (src.IP(), cmd)
        print out
  net.stop()
finally:
  controller.kill()
