# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from htab  import HTab_conv
from racer import Racer_conv





def configureTest():
  test  = RandomHCNFTest(testId         = 'comHTabRacer',
                         # Number of atoms
                         numOfProps     = 3, freqOfProps = 2,
                         numOfNoms      = 0, freqOfNoms  = 0,
                         # Number and depth of modalities
                         numOfRels      = 1,
                         diamDepth      = 2, freqOfDiam  = 2,
                         atDepth        = 0, freqOfAt    = 0,
                         # Test structure
                         batchSize      = 20,
                         fromNumClauses = 10,  # we need 2 clauses at least, because of the hack
                                               # to create formulas with one universal modality part
                         toNumClauses   = 70,
                         step           = 10,
                         timeout        = 20,
                         csize          = "[0,1]"
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  htab    = HTab_conv ('HTab 1.3',            binpath, 'convHTab')
  racer   = Racer_conv('RacerPro 1.9.2 beta', binpath, 'convRacer')

  return [htab, racer]
