# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from hyloresv2  import HyLoResV2
from translator import Translator
from spass      import SPASS





def configureTest():
  test  = RandomHCNFTest(testId         = 'testSTs',
                         # Number of atoms
                         numOfProps     = 5,
                         numOfNoms      = 3,
                         # Number and depth of modalities
                         modalDepth     = 3,
                         # Test structure
                         batchSize      = 10,
                         fromNumClauses = 1,
                         toNumClauses   = 80,
                         step           = 8,
                         timeout        = 10
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  hyloresv2_latest    = HyLoResV2 ('hylores_latest', dirStructure.binDir, 'hylores_latest')
  lthlBin = '/users/led/gorin/repos/lthl/bin'
  spassWithST_sorts   = Translator('st-s1',          lthlBin,             'sthl-spass-plain', SPASS('spass', 1, dirStructure.binDir))
  spassWithST_noSorts = Translator('st-s0',          lthlBin,             'sthl-spass-plain', SPASS('spass', 0, dirStructure.binDir))
  # spassWithLT_c_front = Translator('st_txy', lthlBin, 'sthl-spass-dummyc-tfront', SPASS('spass', dirStructure.binDir))
  # spassWithLT_c_mid   = Translator('st_xty', lthlBin, 'sthl-spass-dummyc-tmid', SPASS('spass', dirStructure.binDir))
  # spassWithLT_c_tback = Translator('st_xyt', lthlBin, 'sthl-spass-dummyc-tback', SPASS('spass', dirStructure.binDir))

  return [hyloresv2_latest, spassWithST_sorts, spassWithST_noSorts]
