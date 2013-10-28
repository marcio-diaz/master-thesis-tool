# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest

from hyloresv2_3 import HyLoResV2_3
from translator import Translator
#from pellet      import PELLET
from fact      import FACT
from htab      import HTab

"""from fact       import FactV2_2"""





def configureTest():
  test  = RandomHCNFTest(testId         = 'testAle',
                         numOfProps     = 5, freqOfProps = 2,
                         numOfNoms      = 3, freqOfNoms  = 2,
                         numOfRels      = 3,
                         maxDepth       = 8,
                         diamDepth      = 4, freqOfDiam  = 2,
                         univDepth      = 4, freqOfUniv  = 2,
                         batchSize      = 40, # 40
                         freqOfAt       = 0, atDepth  = 0,
                         fromNumClauses = 1,
                         toNumClauses   = 51,   # 101
                         step           = 5,
                         timeout        = 50)

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
#  hylores  = HyLoResV2_3('hylores', binpath, 'hylores')
  fact    = Translator('fact', binpath, 'sthl-fact.hs', FACT('FaCTPlusPlus', binpath,'fact-timeout.py'))
#  pellet    = Translator('sthl-rdf', binpath, 'sthl-rdf', PELLET('pellet.sh', binpath))
  htab    = HTab('htab',binpath,'htab')
  return [htab,fact]
