#!/usr/bin/env python
# -*- coding: utf-8 -*-


import sys
import os





paradoxOpt = []
to = False
timeout = None


for arg in sys.argv[1:]:
  if to and not timeout :
    timeout = int(arg)

  elif arg == '--timeout':
    to = True

  else :
    paradoxOpt.append(arg)

print "Flag: " + str(to) 

if to:
  min = str(timeout / 60) 
  sec = str(timeout % 60)
  spaces = " " * (4 - len(min)) 

  stdin, stdout, stderr = os.popen3('timeout 40 /home/bedaride/these/modelbuilder/testing-framework/bin/paradox '+ " ".join(paradoxOpt))

  print stdout.read()[:-1]
  if len(stderr.read())>0:
    print "== Result =================================================================="
    print spaces + min +":" + sec + ".0 | TIMEOUT"

  else:
    stdin, stdout, stderr = os.popen3('timeout 40 /home/bedaride/these/modelbuilder/testing-framework/bin/paradox '+ " ".join(sys.argv[1:]))
    print stdout.read()[:-1]
