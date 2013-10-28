# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from htab        import HTab
from hylotab     import Hylotab
from hyloresv2_3 import HyLoResV2_3
from hyloresv2_5 import HyLoResV2_5





def configureTest():
  test  = RandomHCNFTest(testId         = 'compareAll',
                         # Number of atoms
                         numOfProps     = 2, freqOfProps = 2,
                         numOfNoms      = 2, freqOfNoms  = 2,
                         numOfSVars     = 0, freqOfSVars = 0,
                         # Number and depth of modalities
                         numOfRels      = 2, maxDepth    = 2,
                         diamDepth      = 1, freqOfDiam  = 1,
                         atDepth        = 1, freqOfAt    = 1,
                         downDepth      = 0, freqOfDown  = 0,
                         invDepth       = 0, freqOfInv   = 0,
                         univDepth      = 1, freqOfUniv  = 1,
                         diffDepth      = 0, freqOfDiff  = 0,
                         # Test structure
                         batchSize      = 1,
                         fromNumClauses = 10,
                         toNumClauses   = 12,
                         step           = 2,
                         timeout        = 5,
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  htab    = HTab       ('htab',        binpath, 'htab')
  # hylotab = Hylotab    ('hylotab',     binpath, 'hylotab')
  hylores = HyLoResV2_3('hylores_2.4', binpath, 'hylores')
  # hylores = HyLoResV2_5('hylores_2.4', binpath, 'hylores')

  # return [htab, hylotab, hylores]
  return [htab, hylores]
  # return [htab]
