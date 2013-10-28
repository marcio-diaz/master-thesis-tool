# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest

from hyloresv2_5 import HyLoResV2_5
from spass       import SPASS





def configureTest():
  test  = RandomHCNFTest(testId         = 'serial-vs-parallel',
                         numOfProps     = 4, freqOfProps = 2,
                         numOfNoms      = 3, freqOfNoms  = 2,
                         numOfRels      = 2,
                         diamDepth      = 6, freqOfDiam  = 2,
                         atDepth        = 3, freqOfAt    = 1,
                         batchSize      = 50,
                         fromNumClauses = 20,
                         toNumClauses   = 70,
                         step           = 5,
                         timeout        = 120)

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  hyloresv2_5_ser = HyLoResV2_5('serial', '/home/diegor/tesis/newrepo/hylores/dist/build/hylores', 'hylores')
  hyloresv2_5_par = HyLoResV2_5('parallel-rrobin', '/home/diegor/tesis/newrepo/hylores/dist/build/hylores', 'hylores',3,4,'RROBIN', False)
  hyloresv2_5_par_hash = HyLoResV2_5('parallel-hash', '/home/diegor/tesis/newrepo/hylores/dist/build/hylores', 'hylores',3,4,'HASH', False)

  return [hyloresv2_5_ser, hyloresv2_5_par, hyloresv2_5_par_hash ]
