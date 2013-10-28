# -*- coding: iso-8859-1 -*-
""" eprover module
"""


import re

from prover        import Prover, Statistics, NoParse
from miscFunctions import *


class E(Prover):
    def __init__(self, id, binDir, format, binName = 'eprover', translation = None):
        Prover.__init__(self,id)
        self.__binLocation = '%s/%s' % (binDir, binName)
        self.__format      = format # tptp2 or tptp3
        self.translation = translation
        
    def requiredBinaries(self):
        return [self.__binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        eproverCmdLine  = ['%s' % self.__binLocation,
                           '-s',     # silent
                           '-xAuto', # automatic select clause selection heuristic
                           '-tAuto', # automatic select term ordering
                           '--cpu-limit=%i' % timeout, 
                           '--%s-in' % self.__format, '%s/%s' % (batchDir, testFile)]

        self.runWrapper(eproverCmdLine,
                        testFile,
                        allowedErrorCodes = [6]) # 6 means timeout


    def newStatistics(self):
       return EStatistics()


    def getConfiguration(self):
        return  [("Prover", "eprover"),
                 ("Version", '1.1 "Balasun"'),
                 ("Configuration", "-s -xAuto -tAuto --memory-limit=Auto --%s-in" % self.__format)]


class EStatistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.phaseOneParsers = [self.parseSatUnsatTimeout]

    def addNewColumn(self):
        Statistics.addNewColumn(self)

    def parseResponse(self, responseFile):
        try:
            f        = open(responseFile)
            phaseOne = self.phaseOneParsers[:]
            eof      = False
            while (not eof) and len(phaseOne) > 0:
                line = f.readline()
                eof  = line == ""
                if (not eof):
                    parsed = False
                    for (idx, parser) in enumerate(phaseOne):
                        parsed = parser(line)
                        if parsed: break

                    if parsed:
                        phaseOne[idx:idx + 1] = []

            f.close()

        except:
            print "Error parsing '%s'" % responseFile
            raise

    def isUselessLine(self, line):
        return self.accept(r'^# SZS status.*$|'\
                           r'^# Presaturation interreduction done.*$', 
                           '__uselessLinesParser', line)


    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser  = self.retrieveParser(r'^(# No proof found!)$|'\
                                      r'^(# Proof found!)$|'\
                                      r'^(# Failure: Resource limit exceeded.*)$',
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


    def parseTime(self, timeResponseFile):
        try:
            self._parseTimeSkipping(0, timeResponseFile)

        except NoParse: # in case of timeout
            self._parseTimeSkipping(1, timeResponseFile)


