# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from htab      import HTab
from spartacus import Spartacus
from herod     import Herod





def configureTest():
  test  = RandomHCNFTest(testId         = 'htabSpartacusHerod',
                         # Number of atoms
                         numOfProps     = 2, freqOfProps = 1,
                         numOfNoms      = 3, freqOfNoms  = 1,
                         numOfSVars     = 0, freqOfSVars = 0,
                         # Number and depth of modalities
                         numOfRels      = 1, maxDepth    = 2,
                         diamDepth      = 2, freqOfDiam  = 1,
                         atDepth        = 0, freqOfAt    = 0,
                         downDepth      = 0, freqOfDown  = 0,
                         invDepth       = 0, freqOfInv   = 0,
                         univDepth      = 0, freqOfUniv  = 0,
                         diffDepth      = 0, freqOfDiff  = 0,
                         # Test structure
                         batchSize      = 30,
                         fromNumClauses = 10,
                         toNumClauses   = 100,
                         step           = 10,
                         timeout        = 60,
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  return [  HTab       ('htab', binpath, 'htab', '')
          , Spartacus  ('spartacus', binpath , 'spartacus', '')
          , Herod      ('herod', binpath , 'herod', '')
         ]
