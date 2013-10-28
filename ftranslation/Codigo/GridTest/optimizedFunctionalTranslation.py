# -*- coding: iso-8859-1 -*-
""" optimized functional translation  module
"""

import re

from translation import Translation
from miscFunctions import *

class OptimizedFunctionalTranslation(Translation):
    
    def __init__(self, id, binDir, binName):
        Translation.__init__(self, id)
        self.__binLocation = "%s/%s" % (binDir, binName)

    def run(self, batchDir, testFile, outputFile):
        systemOrDie(self.__binLocation, ['%s/%s' % (batchDir, testFile)
                                         , outputFile])
