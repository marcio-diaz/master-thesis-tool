import re

from prover        import Prover, Statistics
from miscFunctions import *


class Herod(Prover):
    def __init__(self, id, binDir, binName = 'herod', extraArgs=''):
        Prover.__init__(self, id)
        self.binLocation = '%s/%s' % (binDir, binName)
        self.extraArgs   = extraArgs.split()


    def requiredBinaries(self):
        return [self.binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        cmdLine  = ['%s'        % self.binLocation,
                    '-t','%i'    % timeout,
                    '%s/%s' % (batchDir, testFile)]  + self.extraArgs

        self.runWrapper(cmdLine, testFile)

    def newStatistics(self):
       return HerodStatistics()


    def getConfiguration(self):
        return  [("Prover", "herod"),
                 ("Version", "20090718")]


class HerodStatistics(Statistics):
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




    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser  = self.retrieveParser(r'^(satisfiable)$|'\
                                     r'^(unsatisfiable)$|'\
                                     r'^(timeout)$',
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

