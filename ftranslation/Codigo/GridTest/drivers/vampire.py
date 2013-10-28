# -*- coding: iso-8859-1 -*-
""" vampire module
"""


import re

from prover        import Prover, Statistics
from miscFunctions import *


class Vampire(Prover):
    def __init__(self, id, binDir, selection = None, main_alg = None, binName = 'vampire'):
        Prover.__init__(self,id)
        self.__binLocation = '%s/%s' % (binDir, binName)
        if selection:
	  self.__selection = ["--selection", str(selection)]
	else:
	  self.__selection = []

	if main_alg:
	  self.__main_alg = ["--main_alg", main_alg]
	else:
	  self.__main_alg = []


    def requiredBinaries(self):
        return [self.__binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        vampireCmdLine  = ['%s' % self.__binLocation, '--mode', 'vampire'] + self.__main_alg + self.__selection + ['--time_limit', timeout, '%s/%s' % (batchDir, testFile)]

        self.runWrapper(vampireCmdLine, testFile)


    def newStatistics(self):
       return VampireStatistics()


    def getConfiguration(self):
        return  [("Prover", "vampire"),
                 ("Version", "8.0 (7.45 Civatateo)"),
                 ("Configuration", "--mode vampire" + " ".join(self.__main_alg + self.__selection))] 


class VampireStatistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.totalGeneratedClauses = []
        self.retainedClauses       = []


    def addNewColumn(self):
        Statistics.addNewColumn(self)
        self.totalGeneratedClauses.append(0)
        self.retainedClauses.append(0)

    def getStatValues(self):
        temp = Statistics.getStatValues(self)
        temp['totalGeneratedClauses'] = self.totalGeneratedClauses
        temp['retainedClauses']       = self.retainedClauses

        return temp

    def setStatValues(self, stats):
        Statistics.setStatValues(self, stats)
        self.totalGeneratedClauses = temp['totalGeneratedClauses']
        self.retainedClauses       = temp['retainedClauses']


    def parseResponse(self, vampireResponseFile):
        try:
            f = open(vampireResponseFile)
            self.parseResult(f.readline())

            l = f.readline()

            while len(l) > 0 and l != "=== Generated clauses:\n":
                l = f.readline()

            self.replaceLastVal(self.totalGeneratedClauses, self.parseTotal(f.readline()))

            while len(l) > 0 and l != "=== Retained clauses:\n":
                l = f.readline()
            self.replaceLastVal(self.retainedClauses, self.parseTotal(f.readline()))

            f.close()

        except:
            print "Error parsing '%s'" % vampireResponseFile
            raise

    def parseResult(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser = self.retrieveParser(r'^(Satisfiability detected)$|^(Refutation found. Thanks to Tanya!)$|^(Refutation not found)$',
                                     '__satUnsatTimeoutParser')
        matched = parser.search(line)
        assert(matched)
        if   matched.group(1):
            self.replaceLastVal(self.answer, Statistics.ANSWER_SAT)
        elif matched.group(2):
            self.replaceLastVal(self.answer, Statistics.ANSWER_UNSAT)
        elif matched.group(3):
            self.replaceLastVal(self.answer, Statistics.ANSWER_TIMEOUT)
        else: assert(False)


    def parseTotal(self, line):
        parser  = self.retrieveParser(r'^total: (\d+)$', '__totalParser')

        matched = parser.search(line)
        assert(matched)

        return int(matched.group(1))

