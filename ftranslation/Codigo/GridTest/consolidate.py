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

from reports import *





def usage():
  print ("Usage: %s <dir>\n") % (sys.argv[0])
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
  if len(sys.argv) != 2:
    usage()

  workDir = fullabsexpand(sys.argv[1])
  assertIsADirectory(workDir)

  # save current dir and change to the working directory
  cwd = os.getcwd()
  chdirOrDie(workDir)


  # assert directory structure consistency
  assertOne(len(filter(lambda x: x[-3:] == ".py", os.listdir("."))), "original configuration")
  #
  dynamicImport(filter(lambda x: x[-3:] == ".py", os.listdir("."))[0])
  test = configureTest()
  destroyScratch(test)
  #
  assertOne(len(filter(lambda x: x[-7:] == ".tar.gz", os.listdir("."))), "environment tarball")
  nodes = len(filter(lambda x: x[-4:] == ".err", os.listdir(".")))
  assertEq(nodes, len(filter(lambda x: x[-4:] == ".out", os.listdir("."))), "error logs", "output logs")
  assertEq(len(filter(lambda x: x[-7:] == ".pickle", os.listdir("."))), (len(test.provers) + 1) * nodes, "stats and tests", "(# of provers + 1) times the number of error or output logs")

  statsByProver = {}
  for p in test.provers:
    for b in range(1, len(test.batches) + 1):
      p.addNewStatsToBatch(b)

  testFileList = filter(lambda x: x[-7:] == ".pickle" and x[-14:-7] != "__stats", os.listdir("."))
  testList = []

  for f in testFileList:
    testList.append(load(f))

  test.system    = []
  test.host      = []
  test.release   = []
  test.version   = []
  test.machine   = []
  test.processor = []
  test.batchSize = 0

  for t in testList:
    test.system.extend   (t.system)
    test.host.extend     (t.host)
    test.release.extend  (t.release)
    test.version.extend  (t.version)
    test.machine.extend  (t.machine)
    test.processor.extend(t.processor)
    test.batchSize += t.batchSize

    for p in test.provers:
      orig = load(t.testId + "__" + str(p.id) + '__stats.pickle')
      for b in orig.keys():
        s = p.newStatistics()
        s.setStatValues(orig[b])
        p.proverStatsForBatch(b).consolidate(s)

  test.dirStructure.setScratchDir(workDir)
  report = Report(test)
  report.generateFullReport()

  if report.canCompile():
    print "Compiling report..."
    report.compile()

  # leave everything as we found it
  chdirOrDie(cwd)
