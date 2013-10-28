# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from htab        import HTab
from hylotab     import Hylotab
from hyloresv2_3 import HyLoResV2_3





def configureTest():
  test  = RandomHCNFTest(testId         = 'compareHybrid',
                         # Number of atoms
                         numOfProps     = 2, freqOfProps = 2,
                         numOfNoms      = 5, freqOfNoms  = 1,
                         # Number and depth of modalities
                         numOfRels      = 1,
                         diamDepth      = 2, freqOfDiam  = 2,
                         atDepth        = 1, freqOfAt    = 1,
                         # univDepth      = 1, freqOfUniv  = 1,
                         # diffDepth      = 1, freqOfDiff  = 1,
                         # Test structure
                         batchSize      = 30,
                         fromNumClauses = 1,
                         toNumClauses   = 91,
                         step           = 10,
                         timeout        = 20,
                         csize          = "[0,1]"
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  htab    = HTab       ('htab_1.3',    binpath, 'htab')
  hylores = HyLoResV2_3('hylores_2.4', binpath, 'hylores')
  hylotab = Hylotab    ('hylotab',     binpath, 'hylotab')

  return [htab, hylores, hylotab]
