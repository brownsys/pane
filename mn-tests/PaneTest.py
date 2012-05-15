import socket
from subprocess import Popen
from mininet.node import RemoteController, UserSwitch
from mininet.net import Mininet
import mininet.topolib
import mininet.topo
import re
import os, sys
from time import gmtime, strftime, sleep

TreeTopo = mininet.topolib.TreeTopo
LinearTopo = mininet.topo.LinearTopo

class PaneException(Exception):
  pass

class Client(object):

  def __init__(self, user):
    self.__sock__ = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    self.__sock__.connect(('localhost', 4242))
    self.__fd__ = self.__sock__.makefile('rw', 1)
    self.__fd__.write("%s.\n" % user)
    resp = self.__fd__.readline()
    if resp != "logged in\n":
      raise PaneException("login as %s failed; response was %s" % (user,resp))

  def __del__(self):
    self.__fd__.close()
    self.__sock__.close()

  def request(self, req):
    self.__fd__.write("%s\n" % req)
    resp = self.__fd__.readline()
    if resp == "True\n":
      return True
    if resp == "False\n":
      return False
    raise PaneException("request %s; response %s" % (req, resp))

class Test(object):

  def __init__(self, topo):
    os.system('mn -c')
    self.controller = Popen(['./pane', '-p', '4242'])
    sleep(1)
    self.client = Client("root")
    self.net = Mininet(topo=topo,controller=RemoteController,switch=UserSwitch)
    self.net.start()

  def __del__(self):
    if hasattr(self, 'net'):
      self.net.stop()
    if hasattr(self, 'controller'):
      self.controller.kill()

  def ping(self, src, dst, interval, count):
    cmd = 'ping -i %s -c%s %s' % (interval, count, dst.IP())
    out = src.cmd(cmd)
    m = re.search(r"(\d+)% packet loss", out)
    if m == None:
      raise PaneException("%s output was %s" % (cmd, out))
    return int(m.group(1))
  
def time():
  return strftime("%Y-%m-%d %H:%M:%S", gmtime())

def check(actual, expected, message):
  if actual != expected:
    raise AssertionError("%s:\nexpected\n%s\ngot:\n%s" % (message, expected, actual))
