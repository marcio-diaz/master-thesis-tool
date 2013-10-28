import re

from prover        import Prover, Statistics
from miscFunctions import *


class Spartacus(Prover):
    def __init__(self, id, binDir, binName = 'spartacus', extraArgs=''):
        Prover.__init__(self, id)
        self.binLocation = '%s/%s' % (binDir, binName)
        self.extraArgs   = extraArgs.split()


    def requiredBinaries(self):
        return [self.binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        cmdLine  = ['%s'        % self.binLocation,
                    '--timeout=%i'    % timeout,
                    '--intohyloFile=%s/%s' % (batchDir, testFile)]  + self.extraArgs

        self.runWrapper(cmdLine, testFile)

    def newStatistics(self):
       return SpartacusStatistics()


    def getConfiguration(self):
        return  [("Prover", "spartacus"),
                 ("Version", "1.1")]


class SpartacusStatistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.phaseOneParsers = [self.parseSatUnsatTimeout,
                                self.parseCacheHitsCount]
        self.cacheHitsCount            = []

    def addNewColumn(self):
        Statistics.addNewColumn(self)
        self.cacheHitsCount.append(0)

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




    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser  = self.retrieveParser(r'^(satisfiable)$|'\
                                     r'^(unsatisfiable)$|'\
                                     r'(Timeout)$',
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

    def parseCacheHitsCount(self, line):
        return self.__parseMetric(r'Cache hits:      (\d+)$', 'cacheHitsCount', line)

    def __parseMetric(self, pattern, metricAttr, line):
        parser = self.retrieveParser(pattern, '__' + metricAttr + 'Parser')
        matched = parser.search(line)
        if matched:
            val = float(matched.group(1))
            assert(val != None)
            self.replaceLastVal(getattr(self, metricAttr), val)

        return matched != None

