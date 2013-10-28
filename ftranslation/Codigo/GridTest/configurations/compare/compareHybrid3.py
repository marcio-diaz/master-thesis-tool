# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from htab        import HTab
from hyloresv2_3 import HyLoResV2_3





def configureTest():
  test  = RandomHCNFTest(testId         = 'compareHybrid3',
                         # Number of atoms
                         numOfProps     = 2, freqOfProps = 2,
                         numOfNoms      = 2, freqOfNoms  = 2,
                         # Number and depth of modalities
                         numOfRels      = 2,
                         diamDepth      = 2, freqOfDiam  = 2,
                         atDepth        = 2, freqOfAt    = 2,
                         univDepth      = 2, freqOfUniv  = 2,
                         diffDepth      = 0, freqOfDiff  = 0,
                         # Test structure
                         batchSize      = 10,
                         fromNumClauses = 20,
                         toNumClauses   = 40,
                         step           = 2,
                         timeout        = 200,
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  htab    = HTab       ('htab_1.3',    binpath, 'htab')
  hylores = HyLoResV2_3('hylores_2.4', binpath, 'hylores')

  return [htab, hylores]
