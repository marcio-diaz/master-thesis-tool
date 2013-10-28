# -*- coding: iso-8859-1 -*-
""" bliksem module
"""


import re

from prover        import Prover, Statistics
from miscFunctions import *


class Bliksem(Prover):
    def __init__(self, id, binDir, binName = 'bliksem'):
        Prover.__init__(self, id)
        self.__binLocation = '%s/%s' % (binDir, binName)
        self.__timedRun    = '%s/timed-run' % binDir


    def requiredBinaries(self):
        return [self.__binLocation, self.__timedRun]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        timedRunCmd     = [self.__timedRun, timeout]
        bliksemCmdLine  = [self.__binLocation, '%s/%s' % (batchDir, testFile)]

        # untested.timed-run should probably be replaced with something else...
        self.runWrapper(timedRunCmd + ["time"] + timeArgs + bliksemCmdLine,
                        testFile)


    def newStatistics(self):
       return BliksemStatistics()


    def getConfiguration(self):
        return  [("Prover", "bliksem"), ("Version", "1.12")]


class BliksemStatistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)


    def parseResponse(self, spassResponseFile):
        try:
            f = open(spassResponseFile)
            # spawn <cmd> is inserted by the timed-run command
            assert(f.readline().startswith('spawn '))

            for i in range(1,4):
                assert(f.readline().startswith('*** allocated '))

            assert(f.readline() == 'Bliksem 1.12\r\n')

            answerFound = False
            line        = f.readline()
            lineNumber  = 5

            while len(line) > 0 and not answerFound:
                answerFound = self.parseAnswer(line)
                line        = f.readline()
                lineNumber  = lineNumber + 1

            if not answerFound:
                self.replaceLastVal(self.answer, Statistics.ANSWER_TIMEOUT)
                # on timeout, the user time is unreliable (it is the user time
                # of timed-run!) so we replace it with the real time
                self.replaceLastVal(self.usertime, self.realtime[len(self.realtime) - 1])
            else:
                # timed-run sends the output of time to stdout, so we will find it at
                # the end of the current file....
                while len(line) > 0 and not line.startswith('real '):
                    line       = f.readline()
                    lineNumber = lineNumber + 1

                assert(len(line) > 0)
                self._parseTimeSkipping(lineNumber, spassResponseFile)

            f.close()

        except:
            print "Error parsing '%s'" % spassResponseFile
            raise


    def parseAnswer(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable"""
        parser  = self.retrieveParser(r'^found a (saturation)!\r$|^found a (proof)!\r$',
                                      '__satUnsatParser')
        matched = parser.search(line)

        if matched:
            if   matched.group(1):
                self.replaceLastVal(self.answer, Statistics.ANSWER_SAT)
            elif matched.group(2):
                self.replaceLastVal(self.answer, Statistics.ANSWER_UNSAT)

        return matched

