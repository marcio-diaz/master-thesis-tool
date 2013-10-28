# -*- coding: iso-8859-1 -*-
""" Dmtr module
"""


import re

from prover        import Prover, Statistics
from miscFunctions import *


class Hyloban(Prover):
    def __init__(self, id, binDir, binName = 'hyloban', topo = '--t0'):
        Prover.__init__(self, id)
        self.binLocation = '%s/%s' % (binDir, binName)
        self.topo        = topo


    def requiredBinaries(self):
        return [self.binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        cmdLine = ['%s'        % self.binLocation,
                   '-t',  '%i'    % timeout,
                   self.topo,
                   '-f',  '%s/%s' % (batchDir, testFile)]

        self.runWrapper(cmdLine, 
                        testFile,
                    allowedErrorCodes = [1, 2, 3, 256, 512, 768]) # the last 3 numbers are a bash hack


    def newStatistics(self):
       return HylobanStatistics()


    def getConfiguration(self):
        return  [("Prover", "hyloban"),
                 ("Version", "0.2")]


class HylobanStatistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.phaseOneParsers = [self.parseSatUnsatTimeout]


    def addNewColumn(self):
        Statistics.addNewColumn(self)


    def parseResponse(self, htabResponseFile):
        try:
            f        = open(htabResponseFile)
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
                        print "Error unparseable line (phase 1) '%s' in file '%s'" % (line, htabResponseFile)
                        sys.exit(1)

            f.close()

        except:
            print "Error parsing '%s'" % htabResponseFile
            raise


    def isUselessLine(self, line):
        """ Accepts all lines with no relevant statistical information """
        return self.accept(r'^Input:$|'\
                           r'^{.*}$|'\
                           r'^End of input$|'\
                           r'^Axiom:.*$|'\
                           r'^Reading parameters from .*\.hylobanrc$|'\
                           r'^\(final statistics\)$|'\
                           r'^begin$|'\
                           r'^end$|'\
                           r'^\-*$|'\
                           r'^Nodes explored:$|'\
                           r'^Elapsed time:.*$',
                           '__uselessLinesParser', line)


    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser = self.retrieveParser(r'^(SAT)$|^(UNSAT)$|^(TIMEOUT)$',
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


class Hyloban_caching(Hyloban):
    def __init__(self, id, binDir, binName = 'hyloban', topo = "--t0"):
        Hyloban.__init__(self, id, binDir, binName, topo)


    def run(self, batchDir, testFile, timeout):
        timeArgs        = ['-p']
        timeResponse    = self.timeResponse(testFile)
        hyloresCmdLine  = ['%s'        % self.binLocation,
                           '-c',
                           '-t',  '%i'    % timeout,
                           self.topo,
                          '-f',  '%s/%s' % (batchDir, testFile)]
        hyloresResponse = '%s' % self.proverResponse(testFile)

        systemOrDie("time", timeArgs + hyloresCmdLine, stdout = hyloresResponse, stderr = timeResponse,
                    allowedErrorCodes = [1, 2, 3, 256, 512, 768]) # the last 3 numbers are a bash hack

