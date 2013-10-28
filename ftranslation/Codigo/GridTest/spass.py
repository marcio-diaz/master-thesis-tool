# -*- coding: iso-8859-1 -*-
""" spass module
"""


import re

from prover        import Prover, Statistics, NoParse
from miscFunctions import *


class SPASS(Prover):

    def __init__(self, id, binDir, binName = 'SPASS', translation = None, 
                 options = ''):
        Prover.__init__(self,id)
        self.__binLocation = "%s/%s" % (binDir, binName)
        self.translation = translation
        self.options = options

    def requiredBinaries(self):
        return [self.__binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        spassCmdLine  = ['%s' % self.__binLocation,
                         '-Auto=1',        # this is the default, but it doesn't hurt!
                         '-PProblem=0',    # don't print the input clause set
                         '-PGiven=0',      # don't print the given clause
                         '-PStatistic=1',  # show final statistics (default)
                         '-TimeLimit=%i' % timeout
                         ]
        spassCmdLine.extend(self.options.split())
        spassCmdLine.append('%s/%s' % (batchDir, testFile))
        self.spassCmdLine = spassCmdLine
        segFaultCode  = [11]  # SPASS sometimes segfaults when running out of time after printing the results
        self.runWrapper(spassCmdLine, testFile, allowedErrorCodes=segFaultCode)


    def newStatistics(self):
       return SpassStatistics()


    def getConfiguration(self):
        return  [("Prover", "spass"),
                 ("Version", "3.7"),
                 ("Configuration", str(self.spassCmdLine))]
#                 ("Configuration", "-Auto=1 -Sorts=%i" % self.__sortsParam)]


class SpassStatistics(Statistics):
    def __init__(self ):
        Statistics.__init__(self)
        self.phaseOneParsers = [self.parseSatUnsatTimeout]

    def addNewColumn(self):
        Statistics.addNewColumn(self)



    def isUselessLine(self, line):
        return self.accept(r'^#.*memory exhausted.*$|'\
                               r'^.*SPASS is going to crash. This is probably caused by a.*$|'\
                               r'^.*bug in SPASS.  Please send a copy of the input file(s) together.*$|'\
                               r'^.*with the used options to spass@mpi-inf.mpg.de, and someone will.*$|'\
                               r'^.*try to fix the problem.*$|'\
                               r'^.*Command terminated by signal.*$',
                            '__uselessLinesParser', line)

    def parseResponse(self, responseFile):
        try:
            f        = open(responseFile)
            phaseOne = self.phaseOneParsers[:]
            eof      = False
            while (not eof) and len(phaseOne) > 0:
                line = f.readline()
                eof  = line == ""
#                if (not eof) and (not self.isUselessLine(line)):
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
        parser = self.retrieveParser(r'^SPASS beiseite: (Completion) found.$|'\
                                         r'^SPASS beiseite: (Proof) found.$|'\
                                         r'^SPASS beiseite: (Ran out of time).*$',
                                     '__satUnsatTimeoutParser')
        matched = parser.search(line)
        if matched:
           if   matched.group(1):
               self.replaceLastVal(self.answer, Statistics.ANSWER_SAT)
           elif matched.group(2):
               self.replaceLastVal(self.answer, Statistics.ANSWER_UNSAT)
           elif matched.group(3):
               self.replaceLastVal(self.answer, Statistics.ANSWER_TIMEOUT)

        return matched != None

