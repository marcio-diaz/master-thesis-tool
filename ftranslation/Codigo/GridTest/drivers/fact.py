# -*- coding: iso-8859-1 -*-
""" FACT module
"""


import re

from prover        import Prover, Statistics, NoParse
from miscFunctions import *


class FACT(Prover):
    def __init__(self, id, binDir, binName = 'FaCT++'):
        Prover.__init__(self, id)
        self.binLocation = '%s/%s' % (binDir, binName)
        self.__binDir = binDir


    def requiredBinaries(self):
        return [self.binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        cmdLine  = ['%s/timeout.sh' % self.__binDir,
                    '-t','%i'    % timeout,
                    self.binLocation,
                    '%s/TEST.conf'  %  self.__binDir,]

        # Deal with the timeout
	
        self.runWrapper(cmdLine,
                        testFile,
                        allowedErrorCodes=[1,2,3,129,137,143]) # 129,137,143, for kill signals for the timeout
        #if the response is empty, add timeout...

        f = open(self.timeResponse(testFile),'r')
        flines = f.read()
        f.close()
        if re.findall("(Command terminate|Terminate|Kill|Hangup)",flines):
            f = open(self.proverResponse(testFile), 'a')
            f.write("The ' Proof' timed-out\n")
            f.close()

    def newStatistics(self):
       return FACTStatistics()


    def getConfiguration(self):
        return  [("Prover", "FaCT++"),
                 ("Version", "v1.1.10")]


class FACTStatistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.phaseOneParsers = [self.parseSatUnsatTimeout]

    def parseResponse(self, factResponseFile):
        try:

            f        = open(factResponseFile)
            phaseOne = self.phaseOneParsers[:]
            eof      = False
            while (not eof) and len(phaseOne) > 0:
                line = f.readline()
                eof  = line == ""
                if (not eof) and (not self.isUselessLine(line)):
                    parsed = False
                    for (idx, parser) in enumerate(phaseOne):
                        parsed = parser(line)
                        if parsed: break

                    if parsed:
                        phaseOne[idx:idx + 1] = []
                    else:
                        print "Error unparseable line (phase 1) '%s' in file '%s'" % (line, factResponseFile)
                        sys.exit(1)

            f.close()

        except:
            print "Error parsing '%s'" % factResponseFile
            raise


    def isUselessLine(self, line):
        """ Accepts all lines with no relevant statistical information """
        return self.accept(r'^Working time = .*$', '__uselessLinesParser', line)


    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or timed-out """
        parser = self.retrieveParser(r'^The \' Proof\' concept is (satisfiable) w.r.t. TBox$|'\
                                     r'^The \' Proof\' concept is (unsatisfiable) w.r.t. TBox$|'\
                                     r'^The \' Proof\' (timed-out)',
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
            self._parseTimeSkipping(4, timeResponseFile)

        except NoParse:
            # If timeout, then the fourth line will say
            # "Command terminated by signal 1/9/15" or "Terminated/Hangup/Killed" or similar
            self._parseTimeSkipping(5, timeResponseFile)

