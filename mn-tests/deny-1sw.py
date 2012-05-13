#!/usr/bin/python
import socket
from subprocess import Popen
from mininet.node import RemoteController
from mininet.net import Mininet
from mininet.topolib import TreeTopo
from mininet.topo import LinearTopo
import re
import os, sys
from time import gmtime, strftime, sleep

os.system('mn -c')

def time():
  return strftime("%Y-%m-%d %H:%M:%S", gmtime())

def connectToPane():
  sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  sock.connect(('localhost', 4242))
  return sock.makefile('rw', 1)

controller = Popen(['./dnp', '-n', '4242'])
try:
  sleep(1)
  pane = connectToPane()
except e:
  controller.kill()
  print e
  sys.exit(1)

pane.write("root.\n")
resp = pane.readline()
if resp != "logged in\n":
  print "ERROR: could not login to PANE\n%s" % resp
  
  controller.kill()
  sys.exit(1)

def paneResp(expected):
  resp = pane.readline()
  if resp != expected:
    raise AssertionError("expected %s from PANE, got %s" % (expected, resp))

def test(net):
  h1 = net.hosts[0]
  h2 = net.hosts[-1]
  print h1
  print h2
  out = h1.cmd('ping -i 0.5 -c5 %s' % h2.IP())
  print "Pinging from h1 to h2 ..."
  m = re.search(r"(\d+)% packet loss", out)
  if m == None or m.group(1) != "0":
    print "Packets lost during ping:"
    print out
    return
  sleep(1)
  print "Sending deny ..."
  pane.write("deny(srcHost=%s,dstHost=%s) on rootShare from now to +5.\n" % 
              (h1.IP(), h2.IP()))
  paneResp("True\n")
  sleep(1)
  out = h1.cmd('ping -i 0.5 -c5 %s' % h2.IP())
  m = re.search(r"(\d+)% packet loss", out)
  if m == None or m.group(1) != "100":
    print "All packets were not dropped."
    print out
    return
  print "Packets successfully dropped. Sleeping until deny expires ..."
  sleep(4)
  print "Pinging from h1 to h2 ..."
  out = h1.cmd('ping -i 0.5 -c5 %s' % h2.IP())
  m = re.search(r"(\d+)% packet loss", out)
  if m == None or m.group(1) != "0":
    print "Packets lost during ping:"
    print out
    net.interact()
    return
  print "Packets received again."
  print "Test successful."
  


try:
  theTopo = TreeTopo(depth=1,fanout=2)
  net = Mininet(topo=theTopo,controller=RemoteController)
  net.start()
  try:
    test(net)
  finally:
    net.stop()
finally:
  controller.kill()
