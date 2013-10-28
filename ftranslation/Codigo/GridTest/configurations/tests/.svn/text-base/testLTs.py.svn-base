# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from hyloresv2  import HyLoResV2
from translator import Translator
from spass      import SPASS





def configureTest():
  test  = RandomHCNFTest(testId         = 'testLTs',
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
  hyloresv2_latest        = HyLoResV2 ('hylores_latest', dirStructure.binDir, 'hylores_latest')
  lthlBin = '/users/led/gorin/repos/lthl/bin'
  spassWithLT_gnom_tfront = Translator('lt_txy',         lthlBin,             'lthl-spass-gnom-tfront', SPASS('spass', dirStructure.binDir))
  spassWithLT_gnom_tmid   = Translator('lt_xty',         lthlBin,             'lthl-spass-gnom-tmid',   SPASS('spass', dirStructure.binDir))
  spassWithLT_gnom_tback  = Translator('lt_xyt',         lthlBin,             'lthl-spass-gnom-tback',  SPASS('spass', dirStructure.binDir))

  return [hyloresv2_latest, spassWithLT_gnom_tfront, spassWithLT_gnom_tmid, spassWithLT_gnom_tback]
