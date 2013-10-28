# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from hyloban     import Hyloban
from hylotab     import Hylotab_topo





def configureTest():
  test  = RandomHCNFTest(testId         = 'compareHybridTopo',
                         # Number of atoms
                         numOfProps     = 2, freqOfProps = 1,
                         numOfNoms      = 2, freqOfNoms  = 1,
                         # Number and depth of modalities
                         numOfRels      = 1,  # important ! let 1 relation
                         diamDepth      = 1, freqOfDiam  = 1,
                         atDepth        = 1, freqOfAt    = 1,
                         univDepth      = 1, freqOfUniv  = 1,
                         # Test structure
                         batchSize      = 20,
                         fromNumClauses = 1,
                         toNumClauses   = 5,
                         step           = 1,
                         timeout        = 10,
                         csize          = "[0,1]"
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath  = dirStructure.binDir
  topoFlag = "--t0"
  hyloban  = Hyloban     ('hyloban', bindir, "hyloban", topoFlag)
  hylotab  = Hylotab_topo('hylotab', bindir, "hylotab", topoFlag)

  return [hyloban, hylotab]
