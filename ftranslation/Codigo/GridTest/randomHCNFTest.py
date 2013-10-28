# -*- coding: utf-8 -*-
""" RandomHCNFTest module
"""

import sys
import os
import platform


from miscFunctions      import *
from directoryStructure import DirectoryStructure
from reports            import Report
from copy_directory_random import copy_dir_random

class RandomHCNFTest:
  def __init__(self, testId, batchSize, fromNumClauses, toNumClauses, step, timeout,
               numOfProps  = 0, numOfNoms  = 0, numOfSVars  = 0,
               freqOfProps = 0, freqOfNoms = 0, freqOfSVars = 0,
               #
               numOfRels   = 0, maxDepth   = None, forceDepth  = False,
               #
               diamDepth   = 0, atDepth    = 0, downDepth   = 0, invDepth  = 0, univDepth  = 0, diffDepth  = 0,
               freqOfDiam  = 0, freqOfAt   = 0, freqOfDown  = 0, freqOfInv = 0, freqOfUniv = 0, freqOfDiff = 0,
               # Probability of selecting a modality (vs. an atom) as the next
               probOperator = 50,
               # Probability of negating the next modality
               probNegation = 50,
               # this means: "at each level, generate two disjuncts with p(1)"
               csize       = "[0,1,1]",
               #
               generatePartialStats = False,
               #
               system    = None,
               host      = None,
               release   = None,
               version   = None,
               machine   = None,
               processor = None):

    self.testId       = testId.replace(' ', '_')
    self.dirStructure = DirectoryStructure()

    self.batchSize = batchSize

    self.numOfProps = numOfProps
    self.numOfNoms  = numOfNoms
    self.numOfSVars = numOfSVars

    self.freqOfProps = freqOfProps
    self.freqOfNoms  = freqOfNoms
    self.freqOfSVars = freqOfSVars

    self.numOfRels  = numOfRels
    if maxDepth != None:
      self.maxDepth = maxDepth
    else:
      self.maxDepth = diamDepth + atDepth + downDepth

    self.diamDepth = diamDepth
    self.atDepth   = atDepth
    self.downDepth = downDepth
    self.invDepth  = invDepth
    self.univDepth = univDepth
    self.diffDepth = diffDepth

    self.forceDepth = forceDepth

    self.freqOfDiam = freqOfDiam
    self.freqOfAt   = freqOfAt
    self.freqOfDown = freqOfDown
    self.freqOfInv  = freqOfInv
    self.freqOfUniv = freqOfUniv
    self.freqOfDiff = freqOfDiff

    self.probOperator = probOperator
    self.probNegation = probNegation

    self.clauseSizeDistrib = csize

    self.fromNumClauses = fromNumClauses
    self.toNumClauses   = toNumClauses
    self.step           = step
    self.timeout        = timeout
    self.batches        = range(fromNumClauses, toNumClauses + 1, step)

    self.generatePartialStats = generatePartialStats

    self.system    = system
    self.host      = host
    self.release   = release
    self.version   = version
    self.machine   = machine
    self.processor = processor


  def setProvers(self, provers):
    self.provers = provers

  def assertConsistency(self):
    # Test the consistency of the parameters
    assertPositive(self.numOfProps, "number of propositions")
    assertPositive(self.numOfNoms,  "number of nominals")
    assertPositive(self.numOfSVars, "number of state variables")

    assertPositive(self.freqOfProps, "frequency of propositions")
    assertPositive(self.freqOfNoms,  "frequency of nominals")
    assertPositive(self.freqOfSVars, "frequency of state variables")

    assertIsZeroIffIsZero(self.numOfProps, self.freqOfProps, "number of propositions", "frequency of propositions")
    assertIsZeroIffIsZero(self.numOfNoms, self.freqOfNoms,   "number of nominals", "frequency of nominals")
    assertIsZeroIffIsZero(self.numOfSVars, self.freqOfSVars, "number of state variables", "frequency of state variables")

    assertPositive(self.numOfRels, "number of relation symbols")
    assertPositive(self.maxDepth,  "maximum depth")

    assertPositive(self.diamDepth, "diamond depth")
    assertPositive(self.atDepth,   "at depth (nesting)")
    assertPositive(self.downDepth, "downarrow depth (nesting)")
    assertPositive(self.invDepth,  "inverse modality depth")
    assertPositive(self.univDepth, "universal modality depth")
    assertPositive(self.diffDepth, "diff universal modality depth")

    assertPositive(self.freqOfDiam, "frequency of diamonds")
    assertPositive(self.freqOfAt,   "frequency of at")
    assertPositive(self.freqOfDown, "frequency of downarrow")
    assertPositive(self.freqOfInv,  "frequency of inverse modality")
    assertPositive(self.freqOfUniv, "frequency of universal modality")
    assertPositive(self.freqOfDiff, "frequency of diff universal modality")

    assertIsZeroIffIsZero(self.numOfSVars, self.downDepth,                 "number of state variables", "downarrow depth")
    assertIsZeroIffIsZero(self.numOfRels, self.diamDepth,                  "number of relation symbols", "diamond depth")
    assertIsZeroThenIsZero(self.numOfNoms + self.numOfSVars, self.atDepth, "number of nominals and state variables", "at depth")

    assertIsZeroIffIsZero(self.diamDepth, self.freqOfDiam, "diamond depth", "frequency of diamonds")
    assertIsZeroIffIsZero(self.atDepth, self.freqOfAt,     "at depth", "frequency of at")
    assertIsZeroIffIsZero(self.downDepth, self.freqOfDown, "downarrow depth", "frequency of downarrow")
    assertIsZeroIffIsZero(self.invDepth, self.freqOfInv,   "inverse modality depth", "frequency of inverse modality")
    assertIsZeroIffIsZero(self.univDepth, self.freqOfUniv, "universal modality depth", "frequency of universal modality")
    assertIsZeroIffIsZero(self.diffDepth, self.freqOfDiff, "diff universal modality depth", "frequency of diff universal modality")

    assertBetween(self.probOperator, 0, 100, "probability of picking a modal operator")
    assertBetween(self.probNegation, 0, 100, "probability of negating a modal operator")

    assertGtZero(self.batchSize,      "batch size")
    assertGtZero(self.fromNumClauses, "min. number of clauses")
    assertGtZero(self.toNumClauses,   "max. number of clauses")
    assertGtZero(self.step,           "increment")
    assertGtZero(self.timeout,        "timeout")

    if self.fromNumClauses >= self.toNumClauses:
      print 'Error: max. number of clauses < min. number of clauses!'
      sys.exit(1)


  def printOutConfiguration(self):
    print 'Test parameters:              '
    print '  - test id               : %s' % self.testId
    print '  - number of propositions: %i' % self.numOfProps
    print '  - number of nominals    : %i' % self.numOfNoms
    print '  - number of state vars  : %i' % self.numOfSVars
    print '   + freq. of propositions: %i' % self.freqOfProps
    print '   + freq. of nominals    : %i' % self.freqOfNoms
    print '   + freq. of state vars  : %i' % self.freqOfSVars
    print '  - number of relations   : %i' % self.numOfRels
    print '  - max (global) depth    : %i' % self.maxDepth
    print '  - force max depth       : %s' % self.forceDepth
    print '  - diamond depth         : %i' % self.diamDepth
    print '  - at depth              : %i' % self.atDepth
    print '  - downarrow depth       : %i' % self.downDepth
    print '  - inverse modality depth: %i' % self.invDepth
    print '  - univ. modality depth  : %i' % self.univDepth
    print '  - diff. univ. mod. depth: %i' % self.diffDepth
    print '   + freq. of diamonds    : %i' % self.freqOfDiam
    print '   + freq. of at          : %i' % self.freqOfAt
    print '   + freq. of downarrow   : %i' % self.freqOfDown
    print '   + freq. of inverse mod.: %i' % self.freqOfInv
    print '   + freq. of univ. mod.  : %i' % self.freqOfUniv
    print '   + freq. of diff.u. mod.: %i' % self.freqOfDiff
    print '   + clause size distrib. : %s' % self.clauseSizeDistrib
    print '   + prob. of picking mod.: %i' % self.probOperator
    print '   + prob. of negated mod.: %i' % self.probNegation
    print '  - batch size            : %s' % self.batchSize
    print '  - min. number of clauses: %s' % self.fromNumClauses
    print '  - max. number of clauses: %s' % self.toNumClauses
    print '  - increment             : %s' % self.step
    print '  - timeout               : %s' % self.timeout
    print '   + Provers:             :', (map(lambda c: c.id, self.provers))

  def runDir(self, srcDir, batchDir):
    copy_dir_random(srcDir, batchDir, self.batchSize)

  def runHGen(self, clauses, batchDir):
    hgenArgs = ['--num-inst', '%i' % self.batchSize,
                # Number of atoms
                '--prop-vars',  '%i' % self.numOfProps,
                '--nom-vars',   '%i' % self.numOfNoms,
                '--state-vars', '%i' % self.numOfSVars,
                # Frequency of each atom (relative to the frequency of the other atoms)
                '--proba-prop',  '%i' % self.freqOfProps,
                '--proba-nom',   '%i' % self.freqOfNoms,
                '--proba-state', '%i' % self.freqOfSVars,
                # Number and depth of modalities
                '--mods',                '%i' % self.numOfRels,
                '--global-depth',        '%i' % self.maxDepth,
                '--modal-depth',         '%i' % self.diamDepth,
                '--at-depth',            '%i' % self.atDepth,
                '--down-arrow-depth',    '%i' % self.downDepth,
                '--inv-mod-depth',       '%i' % self.invDepth,
                '--univ-mod-depth',      '%i' % self.univDepth,
                '--diff-univ-mod-depth', '%i' % self.diffDepth,
                # Frequency of each modality (relative to the frequency of the other modalities)
                '--proba-mod',  '%i' % self.freqOfDiam,
                '--proba-at',   '%i' % self.freqOfAt,
                '--proba-down', '%i' % self.freqOfDown,
                '--proba-inv',  '%i' % self.freqOfInv,
                '--proba-univ', '%i' % self.freqOfUniv,
                '--proba-diff', '%i' % self.freqOfDiff,
                # Structure of the formulas
                '--clause-size', '%s' % self.clauseSizeDistrib,
                '--num-clauses', '%i' % clauses,
                # Hardcoded
                '--proba-op',  '%i' % self.probOperator,
                '--proba-neg', '%i' % self.probNegation]

    if self.forceDepth:
      hgenArgs.append('--force-depths')

#    hgenArgs.append("--simple")

    cwd = os.getcwd()
    chdirOrDie(batchDir)
#    print " ".join(hgenArgs)
    systemOrDie('"%s"' % self.execHGen, hgenArgs, stdout = None)

    chdirOrDie(cwd)


  def recordSystem(self):
    (system, host, release, version, machine, processor) = platform.uname()
    self.system    = [system]
    self.host      = [host]
    self.release   = [release]
    self.version   = [version]
    self.machine   = [machine]
    self.processor = [processor]


  def run(self):
    self.execHGen = self.dirStructure.binDir + "/hgen"
    assertExecExists(self.execHGen)
    self.__run(parseOnly = False, fromDirectory = False)


  def parse(self):
    self.__run(parseOnly = True)


  def __run(self, parseOnly, fromDirectory):
    self.assertConsistency()

    for prover in self.provers:
      for f in prover.requiredBinaries():
        assertExecExists(f)

      for f in prover.requiredBinaries():
        assertFileExists(f)

    # Run all the provers and save their output
    for (batch, clauses) in enumerate(self.batches):
      batch += 1
      print 'Running batch %i / %i...' % (batch, len(self.batches))

      batchDir  = self.dirStructure.testDirForBatch(batch)
      batchName = self.dirStructure.nameForBatch(batch)

      if not parseOnly:
        mkdirsOrDie(batchDir)
        if fromDirectory:
          self.runDir("/home/mdiaz1/repo/ftranslation/Codigo/GridTest/lwb_k/"+batchName, batchDir)
        else:
          self.runHGen(clauses, batchDir)
      else:
        assertFileExists(batchDir)


      # run the provers, and collect stats
      for prover in self.provers:
        # make a directory for the responses of the prover, and change to it
        proverBatchResponseDir = self.dirStructure.batchResponseDirForProver(prover,batch)
        print "  - %s" % prover.id
        transDir = "%s/%s/%s" % (self.dirStructure.translationsDir,prover.id,batchName)

        if not parseOnly:
          mkdirsOrDie(proverBatchResponseDir)
          mkdirsOrDie(transDir)
          backBatchDir = batchDir
          

          if prover.translation != None:
            prover.translation.runOnBatch(batchDir, transDir)  
            batchDir = transDir
          prover.runOnBatch(batchDir, proverBatchResponseDir, self.timeout)
          batchDir = backBatchDir
        prover.parseBatchOutputFiles(batch, batchDir, proverBatchResponseDir)

      if self.generatePartialStats:
        self.generateStatistics(currentBatch = batch)


  def generateStatistics(self, currentBatch = None):
    chdirOrDie(self.dirStructure.resultsDir)
    report = Report(self, currentBatch)
    report.generateFullReport()
    if report.canCompile():
      print "Compiling report..."
      report.compile()



