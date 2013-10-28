# -*- coding: iso-8859-1 -*-
""" Haksat module
"""

import re
from prover import Prover, Statistics
from miscFunctions import *


class Haksat(Prover):
    def __init__(self,id,binDir,binName='haksat'):
        Prover.__init__(self,id)
        self.binLocation = '%s/%s' % (binDir, binName)

    def requiredBinaries(self):
        return [self.binLocation]

    def requiredFiles(self):
        return []

    def run(self, batchDir, testFile, timeout):
        cmdLine  = ['%s'        % self.binLocation,
                    '-t',  '%i'    % timeout,
                    '-f',  '%s/%s' % (batchDir, testFile)]

        self.runWrapper(cmdLine, testFile,
                    allowedErrorCodes=[1,2,3,256,512,768]) # the last 3 numbers are a bash hack

    def newStatistics(self):
       return HaksatStatistics()

    def getConfiguration(self):
        return  [("Prover", "haksat"),
                 ("Version", "0.0.1")]

class HaksatStatistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.phaseOneParsers = [self.parseSatUnsatTimeout]

    def addNewColumn(self):
        Statistics.addNewColumn(self)

    def parseResponse(self, responseFile):
        try:
            f = open(responseFile)
            phaseOne = self.phaseOneParsers[:]
            eof = False
            while (not eof) and len(phaseOne) > 0:
                line = f.readline()
                eof  = line == ""
                if (not eof):
                    parsed = False
                    for (idx, parser) in enumerate(phaseOne):
                        parsed = parser(line)
                        if parsed: break

                    if parsed: phaseOne[idx:idx+1] = []
            f.close()
        except:
            print "Error parsing '%s'" % responseFile
            raise


    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser = self.retrieveParser(r'^(s SATISFIABLE)$|'\
                                     r'^(s UNSATISFIABLE)$|'\
                                     r'^(s TIMEOUT)$',
                                     '__satUnsatTimeoutParser')
        matched = parser.search(line)
        if matched:
           if   matched.group(1):
               self.replaceLastVal(self.answer, Statistics.ANSWER_SAT)
           elif matched.group(2):
               self.replaceLastVal(self.answer, Statistics.ANSWER_UNSAT)
           elif matched.group(3):
               self.replaceLastVal(self.answer, Statistics.ANSWER_TIMEOUT)
           else: assert(False)

        return matched != None

    def __parseMetric(self, pattern, metricAttr, line):
        parser = self.retrieveParser(pattern, '__' + metricAttr + 'Parser')
        matched = parser.search(line)
        if matched:
            val = float(matched.group(1))
            assert(val != None)
            self.replaceLastVal(getattr(self, metricAttr), val)

        return matched != None




