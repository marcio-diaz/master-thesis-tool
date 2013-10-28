# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from htab        import HTab
from hyloresv2_5 import HyLoResV2_5





def configureTest():
  test  = RandomHCNFTest(testId         = 'ClusterTest 1',
                         # Number of atoms
                         numOfProps     = 10, freqOfProps = 2,
                         numOfNoms      = 10, freqOfNoms  = 2,
                         numOfSVars     = 0, freqOfSVars = 0,
                         # Number and depth of modalities
                         numOfRels      = 3, maxDepth    = 5,
                         diamDepth      = 3, freqOfDiam  = 1,
                         atDepth        = 2, freqOfAt    = 1,
                         downDepth      = 0, freqOfDown  = 0,
                         invDepth       = 0, freqOfInv   = 0,
                         univDepth      = 0, freqOfUniv  = 0,
                         diffDepth      = 0, freqOfDiff  = 0,
                         # Test structure
                         batchSize      = 200,
                         fromNumClauses = 10,
                         toNumClauses   = 500,
                         step           = 20,
                         timeout        = 60,
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  htab    = HTab       ('htab',        binpath, 'htab')
  hylores = HyLoResV2_5('hylores_2.5', binpath, 'hylores')

  return [htab, hylores]
