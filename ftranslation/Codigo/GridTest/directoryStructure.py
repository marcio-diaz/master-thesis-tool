# -*- coding: utf-8 -*-
""" DirectoryStructure module
"""


import os
import tempfile





class DirectoryStructure:
  def __init__(self):
    self.initialDir   = os.getcwd()
    self.binDir       = self.initialDir + '/bin'
    self.scratchDir   = None
    self.testsDir     = None
    self.responsesDir = None
    self.resultsDir   = None
    self.translationsDir = None

  def reinit(self):
    self.initialDir   = os.getcwd()
    self.binDir       = self.initialDir + '/bin'
    self.scratchDir   = None
    self.testsDir     = None
    self.responsesDir = None
    self.resultsDir   = None
    self.translationsDir = None

  def setScratchDir(self, newScratchDir):
    if newScratchDir[-1] == '/':
      newScratchDir = newScratchDir[:-1]

    self.scratchDir   = newScratchDir
    self.testsDir     = self.scratchDir + '/tests'
    self.responsesDir = self.scratchDir + '/responses'
    self.resultsDir   = self.scratchDir + '/results'
    self.translationsDir   = self.scratchDir + '/translations'

  def setNewScratchDirFor(self, testId):
    self.setScratchDir(tempfile.mkdtemp(prefix = 'scratch-%s.' % testId, dir = self.initialDir))


  def nameForBatch(self, batch):
    return 'batch-%s' % str(batch)


  def testDirForBatch(self, batch):
    return '%s/%s' % (self.testsDir, self.nameForBatch(batch))


  def responsesDirForProver(self, prover):
    return '%s/%s' % (self.responsesDir, prover.id)


  def batchResponseDirForProver(self, prover, batch):
    return '%s/%s' % (self.responsesDirForProver(prover), self.nameForBatch(batch))
