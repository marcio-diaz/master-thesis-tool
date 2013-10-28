# -*- coding: iso-8859-1 -*-
""" Hylotab module
"""


import re

from prover        import Prover, Statistics
from miscFunctions import *


class Hylotab(Prover):
    def __init__(self,id,binDir,binName='hylotab'):
        Prover.__init__(self,id)
        self.binLocation = '%s/%s' % (binDir, binName)


    def requiredBinaries(self):
        return [self.binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        cmdLine  = ['%s'           % self.binLocation,
                    '-t' , '%i'    % timeout,
                    '-f' , '%s/%s' % (batchDir, testFile)]

        self.runWrapper(cmdLine, 
                        testFile,
                    allowedErrorCodes=[1,2,3,256,512,768]) # the last 3 numbers are a bash hack


    def newStatistics(self):
       return HylotabStatistics()


    def getConfiguration(self):
        return  [("Prover", "hylotab"),
                 ("Version", "1.1")]


class HylotabStatistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.phaseOneParsers = [self.parseSatUnsatTimeout]


    def addNewColumn(self):
        Statistics.addNewColumn(self)


    def parseResponse(self, hylotabResponseFile):
        try:
            f = open(hylotabResponseFile)
            phaseOne = self.phaseOneParsers[:]
            eof = False
            while (not eof) and len(phaseOne) > 0:
                line = f.readline()
                eof  = line == ""
                if (not eof) and (not self.isUselessLine(line)):
                    parsed = False
                    for (idx, parser) in enumerate(phaseOne):
                        parsed = parser(line)
                        if parsed: break

                    if parsed:
                        phaseOne[idx:idx+1] = []
                    else:
                        print "Error unparseable line (phase 1) '%s' in file '%s'" % (line, hylotabResponseFile)
                        sys.exit(1)

            f.close()

        except:
            print "Error parsing '%s'" % hylotabResponseFile
            raise


    def isUselessLine(self, line):
        """ Accepts all lines with no relevant statistical information """
        return self.accept(r'^$|'\
                           r'^Input:$|'\
                           r'^{.*}$|'\
                           r'^End of input$|'\
                           r'^Axioms:$|'\
                           r'^\[.*$|'\
                           r'^Elapsed time:.*$',
                           '__uselessLinesParser', line)


    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser = self.retrieveParser(r'^(satisfiable).*$|^(not satisfiable).*$|^(TIMEOUT)$',
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


class Hylotab_s4(Hylotab):
    def __init__(self, id, binDir, binName = 'hylotab'):
        Hylotab.__init__(self, id, binDir, binName)


    def run(self, batchDir, testFile, timeout):
        timeArgs        = ['-p']
        timeResponse    = self.timeResponse(testFile)
        hyloresCmdLine  = ['%s'           % self.binLocation,
                           '-t' , '%i'    % timeout,
                           ' --s4 ',
                           '-f' , '%s/%s' % (batchDir, testFile)]
        hyloresResponse = '%s' % self.proverResponse(testFile)

        systemOrDie("time", timeArgs + hyloresCmdLine, stdout = hyloresResponse, stderr = timeResponse,
                    allowedErrorCodes = [1, 2, 3, 256, 512, 768])  # the last 3 numbers are a bash hack


class Hylotab_topo(Hylotab):
    def __init__(self, id, binDir, binName = 'hylotab', topo = "--t0"):
        Hylotab.__init__(self, id, binDir, binName)
        self.topo = topo


    def run(self, batchDir, testFile, timeout):
        timeArgs        = ['-p']
        timeResponse    = self.timeResponse(testFile)
        hyloresCmdLine  = ['%s'           % self.binLocation,
                           '-t' , '%i'    % timeout,
                           self.topo,
                           '-f' , '%s/%s' % (batchDir, testFile)]

        hyloresResponse = '%s' % self.proverResponse(testFile)

        systemOrDie("time", timeArgs + hyloresCmdLine, stdout = hyloresResponse, stderr = timeResponse,
                    allowedErrorCodes = [1, 2, 3, 256, 512, 768])  # the last 3 numbers are a bash hack

