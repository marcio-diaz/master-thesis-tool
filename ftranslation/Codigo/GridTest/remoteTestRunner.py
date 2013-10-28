#!/usr/bin/env python
# -*- coding: utf-8 -*-


import sys
import os
import platform

from miscFunctions import *





def usage():
    print ("Usage: %s <test[.pickle]> <home>\n" % sys.argv[0])
    sys.exit(1)





######################
#  M A I N           #
######################
if __name__ == "__main__":
  if len(sys.argv) != 3:
    usage()

  fileName      = fullexpand(sys.argv[1])
  home          = fullexpand(sys.argv[2])
  (_, temphome) = os.path.split(home)
  # MUST change directories BEFORE loading, otherwise python won't find the modules needed
  chdirOrDie(home)

  test = load(fileName)

  (_, host, _, _, _, _) = platform.uname()
  systemOrDie('echo [1 of 7 --- %s :: remoteTestRunner --- %s] remoteTestRunner started | tee -a logfile;' % (nowString(), host), [])

  # make temp directory
  systemOrDie('mkdir', ['/tmp/%s' % temphome])
  systemOrDie('echo [2 of 7 --- %s :: remoteTestRunner --- %s] Created temporary home at %s | tee -a logfile;' % (nowString(), host, temphome), [])
  # extract tarball to temp directory...
  systemOrDie('tar', ['xzf', fullexpand('./environment.tar.gz'), '-C', '/tmp/%s' % temphome])
  systemOrDie('echo [3 of 7 --- %s :: remoteTestRunner --- %s] Environment tarball extracted | tee -a logfile;' % (nowString(), host), [])
  # ...and go there
  chdirOrDie('/tmp/%s' % temphome)

  # run the testRunner leaving error and output logs in the environment's home
  systemOrDie('echo [4 of 7 --- %s :: remoteTestRunner --- %s] testRunner launched | tee -a %s/logfile;' % (nowString(), host, home), [])
  systemOrDie('./testRunner.py',
              [fileName, 'noreport'],
               stdout = '%s/%s' % (home, test.testId + '.out'),
               stderr = '%s/%s' % (home, test.testId + '.err'))
  systemOrDie('echo [5 of 7 --- %s :: remoteTestRunner --- %s] testRunner finished | tee -a %s/logfile;' % (nowString(), host, home), [])

  # reload the test with the test configuration which actually got to run
  test = load('%s.pickle' % test.testId)
  systemOrDie('echo [6 of 7 --- %s :: remoteTestRunner --- %s] Test reloaded | tee -a %s/logfile;' % (nowString(), host, home), [])

  # check number of stats
  assertEq(len(filter(lambda x: x[-14:] == "__stats.pickle", os.listdir(test.dirStructure.scratchDir))), len(test.provers), "number of generated statistics files", "number of provers")

  # copy the stats to the environment's home
  systemOrDie('cp -f %s/*__stats.pickle' % test.dirStructure.scratchDir, [home])
  # copy resulting test pickle to the environment's home
  systemOrDie('cp', ['-f', '/tmp/%s/%s.pickle' % (temphome, test.testId),   home])

  # get to the tests directory
  chdirOrDie(test.dirStructure.testsDir)
  # prepend the test id
  systemOrDie('for i in *; do cd $i; for j in *; do mv $j %s-$j; done; cd ..; done' % test.testId, [])
  # copy the tests to the environment's home
  systemOrDie('cp', ['-rf', test.dirStructure.testsDir, home])

  # create DONE token for this node
  systemOrDie('echo [7 of 7 --- %s :: remoteTestRunner --- %s] DONE token created | tee -a %s/logfile;' % (nowString(), host, home), [])
  systemOrDie('touch', ['%s/DONE.%s' % (home, test.testId)])
