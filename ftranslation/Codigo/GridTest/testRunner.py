#!/usr/bin/env python
# -*- coding: utf-8 -*-


from miscFunctions import *

# this line extends the modules search path in order to find the prover's drivers in the "drivers" directory
import sys
sys.path.append(fullabsexpand('drivers'))

import os





def usage():
  print ("Usage: %s <testConfiguration> [noreport]\n"
         "   or: %s <scratchDir> [noreport]]") % (sys.argv[0],sys.argv[0])
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
    if len(sys.argv) != 3 or sys.argv[2] != "noreport":
      usage()

  fileName = fullexpand(sys.argv[1])

  if isADirectory(fileName):
    parseOnly = True
    test      = load('%s/test.pickle' % sys.argv[1])

  else:
    parseOnly = False
    if fileName[-7:] == ".pickle":
      test = load(fileName)
      # reset and create directory structure for test
      test.dirStructure.reinit()
      test.dirStructure.setNewScratchDirFor(test.testId)

    else:
      dynamicImport(fileName)
      # we expect the definition of a function configureTest()
      test = configureTest()

  test.recordSystem()

  # save test with testId in the initial directory
  save(test, test.dirStructure.initialDir + ('/%s.pickle' % test.testId))
  # save test with default name in the scratch directory
  save(test, test.dirStructure.scratchDir + ('/test.pickle'))

  # Test file structure
  if not parseOnly:
    mkdirsOrDie(test.dirStructure.testsDir)
    mkdirsOrDie(test.dirStructure.responsesDir)
    mkdirsOrDie(test.dirStructure.translationsDir)

  else:
    assertFileExists(test.dirStructure.testsDir)
    assertFileExists(test.dirStructure.responsesDir)
    assertFileExists(test.dirStructure.translationsDir)

  if not os.access(test.dirStructure.resultsDir, os.F_OK):
    mkdirsOrDie(test.dirStructure.resultsDir)

  test.printOutConfiguration()

  if parseOnly:
    print 'Parsing only!'
    test.parse()

  else:
    test.run()

  # save statistics
  for p in test.provers:
    stat = {}
    for b in range(1, len(test.batches) + 1):
      stat[b] = p.proverStatsForBatch(b).getStatValues()
    # save statistics both on the scratch directory and on the initial one for ease of access
    save(stat, test.dirStructure.scratchDir + '/' + test.testId + "__" + str(p.id) + '__stats.pickle')

  # finally, generate the results
  if not (len(sys.argv) == 3 and sys.argv[2] == "noreport"):
    test.generateStatistics()
