# -*- coding: iso-8859-1 -*-
""" translation module, defines the Translation class
"""

import os
import sys
import re

from miscFunctions import *

class Translation:
    def __init__(self, id):
        self.id = str(id)

        

    def runOnBatch(self, batchDir, translationDir):
        initialDir = os.getcwd()
        chdirOrDie(translationDir)
        
        testFiles = os.listdir(batchDir)
        testFiles.sort()
        for testFile in sortedListDir(batchDir):
            outputFile = "%s" % testFile
            self.run(batchDir, testFile, outputFile)
    
        chdirOrDie(initialDir)


    def run(self, bacthDir, testFile):
        assert(False)
        
