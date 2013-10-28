# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest


from hyloresv2_1 import HyLoResV2_1
# from hyloresv2   import HyLoResV2
from translator  import Translator
from spass       import SPASS





def configureTest():
  test  = RandomHCNFTest(testId         = 'gran-corrida-lt-st-hlr-p3-n0-d3',
                         # Number of atoms
                         numOfProps     = 3, freqOfProps = 2,
                         # numOfNoms      = 3, freqOfNoms  = 2,
                         # Number and depth of modalities
                         numOfRels      = 1,
                         diamDepth      = 3, freqOfDiam  = 2,
                         # atDepth        = 2, freqOfAt    = 2,
                         # Test structure
                         batchSize      = 20,
                         fromNumClauses = 1,
                         toNumClauses   = 201,
                         step           = 20,
                         timeout        = 2 * 60
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  hyloresv2_1_latest      = HyLoResV2_1('hylores2.1', dirStructure.binDir, 'hylores_latest')
  # hyloresv2_darcs_initial = HyLoResV2('hylores2', '/users/led/gorin/testing/testing-framework/bin', 'hylores_darcs_initial_import')

  lthlBin = '/users/led/gorin/repos/lthl/bin'
  spassWithST_plain       = Translator('st', lthlBin, 'sthl-spass-plain', SPASS('spass', 0, dirStructure.binDir))
  spassWithLT_gnom_tfront = Translator('lt_txy', lthlBin, 'lthl-spass-gnom-tfront', SPASS('spass', 0, dirStructure.binDir))

  return [hyloresv2_1_latest, spassWithST_plain, spassWithLT_gnom_tfront]
