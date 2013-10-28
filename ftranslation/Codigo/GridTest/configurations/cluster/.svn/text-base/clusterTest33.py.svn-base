# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from htab        import HTab
from hyloresv2_3 import HyLoResV2_3





def configureTest():
  test  = RandomHCNFTest(testId         = 'ClusterTest 33',
                         # Number of atoms
                         numOfProps     = 10, freqOfProps = 2,
                         numOfNoms      = 10, freqOfNoms  = 2,
                         numOfSVars     = 0, freqOfSVars = 0,
                         # Number and depth of modalities
                         numOfRels      = 3, maxDepth    = 10,
                         diamDepth      = 10, freqOfDiam  = 1,
                         atDepth        = 0, freqOfAt    = 0,
                         downDepth      = 0, freqOfDown  = 0,
                         invDepth       = 0, freqOfInv   = 0,
                         univDepth      = 0, freqOfUniv  = 0,
                         diffDepth      = 0, freqOfDiff  = 0,
                         # Test structure
                         batchSize      = 2,
                         fromNumClauses = 200,
                         toNumClauses   = 210,
                         step           = 10,
                         timeout        = 100
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  htab    = HTab       ('htab',        binpath, 'htab')
  hylores = HyLoResV2_3('hylores_2.4', binpath, 'hylores')

  return [htab, hylores]
