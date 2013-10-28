#!/usr/bin/env python
# -*- coding: utf-8 -*-


from miscFunctions import *

# this line extends the modules search path in order to find the prover's drivers in the "drivers" directory
import sys
sys.path.append(fullabsexpand('drivers'))

import os
import platform
import math
import tempfile
import time





def usage():
  print ("Usage: %s <config> <timeout> <home>\n") % (sys.argv[0])
  sys.exit(1)



def dynamicImport(fileName):
  try:
    __doDynamicImport(fileName)

  except IOError:
    try:
      __doDynamicImport(fileName + '.py')

    except IOError:
      raise IOError('File not found "%s" (nor "%s.py")' % (fileName, fileName))


def __doDynamicImport(fileName):
  f = open(fileName)
  exec(f) in globals()
  f.close()





######################
#  M A I N           #
######################
if __name__ == "__main__":
  # set start time
  start = time.time()

  if len(sys.argv) != 4:
    usage()

  systemOrDie('echo [1 of 7 --- %s :: deployer] Deployer started | tee -a logfile;' % nowString(), [])

  fileName = fullexpand(sys.argv[1])
  timeout  = int(sys.argv[2])
  home     = fullexpand(sys.argv[3])
  assertGtZero(timeout, 'timeout')

  chdirOrDie(home)

  # declare and check interval for testing termination of the nodes (in seconds)
  sleepTime = 2
  assertGtZero(sleepTime, 'sleep time')

  # initialize empty dictionary
  nodeList = {}
  # open nodefile...
  f = open(os.environ['OAR_NODEFILE'])
  # ...and iterate through its contents getting node's fqdns and multiplicity
  for line in f:
    setinc(nodeList, line[0:-1], 0, 1)

  # report available nodes
  systemOrDie('echo [2 of 7 --- %s :: deployer] Assigned nodes:     | tee -a logfile;' % nowString(),                         [])
  for node in nodeList.keys():
    systemOrDie('echo [2 of 7 --- %s :: deployer] ..Node: %s --- %i | tee -a logfile;' % (nowString(), node, nodeList[node]), [])


  # set number of unique nodes and check for bullshit
  nodes = len(nodeList.keys())
  assertGtZero(nodes, 'assigned nodes')

  dynamicImport(fileName)
  # we expect the definition of a function configureTest()
  test = configureTest()
  # we don't want any scratch for this test, only its config
  destroyScratch(test)

  # calculate new batch size
  test.batchSize = int(math.ceil(float(test.batchSize) / float(nodes)))
  # save original testId
  originalTestId = test.testId

  # create stub tests
  for node in nodeList.keys():
    # rename test...
    test.testId = originalTestId + "__" + node
    # ...and save it
    save(test, "%s.pickle" % test.testId)
    systemOrDie('echo [3 of 7 --- %s :: deployer] Created stub test for %s | tee -a logfile;' % (nowString(), node), [])

    # launch remote tests
    os.system('( oarsh %s %s/remoteTestRunner.py %s/%s.pickle %s &>/dev/null ) &' % (node, home, home, test.testId, home))
    systemOrDie('echo [4 of 7 --- %s :: deployer] Initiated remote test runner on %s | tee -a logfile;' % (nowString(), node), [])


  # check for the stats every sleepTime seconds
  systemOrDie('echo [5 of 7 --- %s :: deployer] Waiting on the processing nodes | tee -a logfile;' % nowString(), [])
  while ((time.time() - start) < timeout) and (nodes > len(filter(lambda x: x[:5] == "DONE.", os.listdir(".")))):
    time.sleep(sleepTime)

  systemOrDie('echo [6 of 7 --- %s :: deployer] Processing nodes finished | tee -a logfile;' % nowString(), [])

  # create DONE token
  systemOrDie('echo [7 of 7 --- %s :: deployer] DONE token created | tee -a logfile;' % nowString(), [])
  systemOrDie('touch ./DONE', [])
