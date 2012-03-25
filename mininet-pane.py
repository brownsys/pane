#!/usr/bin/python
from subprocess import Popen
from mininet.node import RemoteController
from mininet.net import Mininet
from mininet.topolib import TreeTopo
from mininet.topo import LinearTopo
import re
import os
from time import gmtime, strftime

def time():
  return strftime("%Y-%m-%d %H:%M:%S", gmtime())

os.system('mn -c')
controller = Popen(['./dnp', '-n', '4242'])
try:
  theTopo = TreeTopo(depth=3,fanout=2)
  #theTopo = LinearTopo(k=2)
  net = Mininet(topo=theTopo,controller=RemoteController)
  net.start()
  print "Pinging started at %s " % time()
  for src in net.hosts:
    for dst in net.hosts:
      if src == dst:
        continue
      cmd = 'ping -i 1 -c3 %s' % dst.IP()
      print '%s$ %s' % (src.IP(), cmd)
      out = src.cmd(cmd)
      m = re.search(r"(\d+% packet loss)", out)
      #if True:
      print m.group(1)
  print "Pinging finished at %s " % time()
  net.stop()
finally:
  controller.kill()
