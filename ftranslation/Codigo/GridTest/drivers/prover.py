# -*- coding: iso-8859-1 -*-
""" prover module, defines the Prover class
"""


import os
import sys
import re

from miscFunctions import *


class Prover:
    def __init__(self, id):
        self.id                  = str(id)
        self.__statisticsByBatch = {}
        self.__timeCmd           = configureTime()


    """ subclasses must return a list with the files they need to execute
    """
    def requiredBinaries(self):
        assert(False)


    """ subclasses must return a list with the files they need (configuration files, etc)
    """
    def requiredFiles(self):
        assert(False)


    def proverStatsForBatch(self, batch):
        return self.__statisticsByBatch[batch]


    def sortedStatsKeys(self):
        k = self.__statisticsByBatch.keys()
        k.sort()
        return k


    def isPreprocessor(self):
        return False


    """ to be supplied by subclasses
    """
    def getConfiguration(self):
        assert(False)


    def runOnBatch(self, batchDir, responseDir, timeout):
        initialDir = os.getcwd()
        chdirOrDie(responseDir)
        

        testFiles = os.listdir(batchDir)
        testFiles.sort()
        for testFile in sortedListDir(batchDir):
            self.run(batchDir, testFile, timeout)

        chdirOrDie(initialDir);


    """ to be supplied by subclasses
    """
    def run(self, batchDir, testFile, timeout):
        assert(False)

    def runWrapper(self, cmdLine, testFile, allowedErrorCodes = []):
        timeResponse   = self.timeResponse(testFile)
        proverResponse = '%s' % self.proverResponse(testFile)

        timeProg = self.__timeCmd[0]
        timeArgs = self.__timeCmd[1:]

        systemOrDie(timeProg, timeArgs + cmdLine,
                    allowedErrorCodes = allowedErrorCodes,
                    stdout            = proverResponse,
                    stderr            = timeResponse)


    def parseBatchOutputFiles(self, batch, batchDir, responseDir):
        initialDir = os.getcwd()
        chdirOrDie(responseDir)

        # Create the statistics object for this number of clauses
        self.addNewStatsToBatch(batch)

        for testFile in sortedListDir(batchDir):
            self.addNewColumnToStatsFor(batch)
            self.parseOutputFilesFor(batch, testFile)

        chdirOrDie(initialDir)


    def addNewStatsToBatch(self, batch):
        newStats = self.newStatistics()
        self.__statisticsByBatch[batch] = newStats
        return newStats


    def addNewColumnToStatsFor(self, batch):
        self.__statisticsByBatch[batch].addNewColumn()


    def newStatistics(self):
        return Statistics()


    def parseOutputFilesFor(self, batch, testFile):
        stats = self.proverStatsForBatch(batch)
        stats.parseTime(self.timeResponse(testFile))
        stats.parseResponse(self.proverResponse(testFile))
        stats.parseStats(self.proverStat(testFile))


    def timeResponse(self, testFile):
        return testFile + '.time'


    def proverResponse(self, testFile):
        return testFile + '.response'

    def proverStat(self, testFile):
        return testFile + '.stat'

class Statistics:
    def __init__(self):
        self.realtime = []
        self.usertime = []
        self.systime  = []
        self.answer   = []
        self.__realTimeParser = re.compile(r'^real\s*(\d+\.\d+)\s*$')
        self.__userTimeParser = re.compile(r'^user\s*(\d+\.\d+)\s*$')
        self.__sysTimeParser  = re.compile(r'^sys\s*(\d+\.\d+)\s*$')


    def addNewColumn(self):
        self.realtime.append(None)
        self.usertime.append(None)
        self.systime.append(None)
        self.answer.append(None)


    def getStatValues(self):
        temp = {}
        temp['realtime'] = self.realtime
        temp['usertime'] = self.usertime
        temp['systime']  = self.systime
        temp['answer']   = self.answer

        return temp

    def setStatValues(self, stats):
        self.realtime = stats['realtime']
        self.usertime = stats['usertime']
        self.systime  = stats['systime']
        self.answer   = stats['answer']

    def consolidate(self, stats):
        self.consolidateTheseLists(stats, self.getStatValues().keys())

    def consolidateTheseLists(self, stats, fieldNames):
        for s in fieldNames:
            getattr(self, s).extend(getattr(stats, s))

    def parseTime(self, timeResponseFile):
        self._parseTimeSkipping(0, timeResponseFile)


    def _parseTimeSkipping(self, linesToSkip, timeResponseFile):
        try:
            f = open(timeResponseFile, 'r')
            for _ in range(linesToSkip):
                f.readline()

            self.replaceLastVal(self.realtime, float(self.__realTimeParser.search(f.readline()).group(1)))
            self.replaceLastVal(self.usertime, float(self.__userTimeParser.search(f.readline()).group(1)))
            self.replaceLastVal(self.systime,  float(self.__sysTimeParser.search(f.readline()).group(1)))
            f.close()

        except:
            raise NoParse("Error parsing '%s'" % timeResponseFile)


    """ to be supplied by subclasses
    """
    def parseResponse(self, timeResponseFile):
        assert(False)

    def parseStats(self, statFile):
        """ to be supplied by subclasses
        """

    def replaceLastVal(self, l, val):
        l[-1] = val


    def retrieveParser(self, pattern, attribute):
        if not hasattr(self, attribute):
            setattr(self, attribute, re.compile(pattern))

        return getattr(self, attribute)


    def accept(self, pattern, parserAttr, line):
        parser = self.retrieveParser(pattern, parserAttr)
        return parser.search(line) != None

    INFINITY       = sys.maxint

    ANSWER_SAT     =  1
    ANSWER_UNSAT   =  0
    ANSWER_TIMEOUT = -1


class NoParse(Exception):
    def __init__(self, value):
        self.value = value


    def __str__(self):
        return repr(self.value)

