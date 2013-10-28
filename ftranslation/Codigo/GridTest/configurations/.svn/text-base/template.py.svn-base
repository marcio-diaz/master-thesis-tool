# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


#from htab        import HTab
#from hyloresv2_3 import HyLoResV2_3
from drivers.spass import SPASS




def configureTest():
  test  = RandomHCNFTest(testId         = 'Template',
                         # Number of atoms
                         numOfProps     = 2, freqOfProps = 2,
                         numOfNoms      = 2, freqOfNoms  = 2,
                         numOfSVars     = 0, freqOfSVars = 0,
                         # Number and depth of modalities
                         numOfRels      = 2, maxDepth    = 2,
                         diamDepth      = 1, freqOfDiam  = 1,
                         atDepth        = 1, freqOfAt    = 1,
                         downDepth      = 1, freqOfDown  = 1,
                         invDepth       = 0, freqOfInv   = 0,
                         univDepth      = 0, freqOfUniv  = 0,
                         diffDepth      = 0, freqOfDiff  = 0,
                         # Test structure
                         batchSize      = 5,
                         fromNumClauses = 10,
                         toNumClauses   = 20,
                         step           = 2,
                         timeout        = 10,
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  spass = SPASS('spass', binpath, 'spass')
#  htab    = HTab       ('htab',        binpath, 'htab')
#  hylores = HyLoResV2_3('hylores_2.4', binpath, 'hylores')

#  return [htab, hylores]
  return [spass]
