# -*- coding: utf-8 -*-


from randomHCNFTest import RandomHCNFTest

from spartacus   import Spartacus
from htab        import HTab
from hyloresv2_5 import HyLoResV2_5
from translator  import Translator
from spass       import SPASS
from eprover     import E

def configureTest():
  test  = RandomHCNFTest(testId         = 'megacorrida-lt-st-hlr-p3-n0-d2_htab',
                         # Number of atoms
                         numOfProps     = 9, freqOfProps = 2,
                         numOfNoms      = 9, freqOfNoms  = 2,
                         # Number and depth of modalities
                         numOfRels      = 1,
                         diamDepth      = 2, freqOfDiam  = 2,
                         # atDepth        = 2, freqOfAt    = 2,
                         # Test structure
                         batchSize      = 9, # 50,
                         fromNumClauses = 1,
                         toNumClauses   = 31, #101,
                         step           = 10, #5,
                         timeout        = 5 #30
                        )

  test.dirStructure.setNewScratchDirFor(test.testId)
  test.setProvers(proverConfiguration(test.dirStructure))
  return test


def proverConfiguration(dirStructure):
  binpath = dirStructure.binDir
  hylores     = HyLoResV2_5('hylores2.5', binpath, 'hylores')
  htab        = HTab       ('htab',        binpath, 'htab')
  spartacus   = Spartacus  ('spartacus', binpath , 'spartacus', '')
  spassST     = Translator ('st', binpath, 'sthl-tptp', SPASS('spass', 0, binpath))
  spassLT     = Translator ('lt_txy', binpath, 'lthl-tptp', SPASS('spass', 0, binpath))
  eST         = Translator ('st', binpath, 'sthl-tptp', E('e', binpath, "tptp3" ))
  eLT         = Translator ('lt_txy', binpath, 'lthl-tptp', E('e', binpath, "tptp3" ))

  return [htab, hylores, spartacus, spassST, spassLT, eST, eLT]
