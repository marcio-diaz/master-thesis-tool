# -*- coding: iso-8859-1 -*-
""" HyLoResV2_5 module
"""


import re
import os

from prover        import Statistics, NoParse
from hyloresv2     import HyLoResV2
from miscFunctions import *


class HyLoResV2_5(HyLoResV2):
    def __init__(self, id,
                 binDir, binName = 'hylores',
                 workers = 0, threads = 4,
                 dispatchAlgStr = 'RROBIN',
                 runtimeStats = False):

        HyLoResV2.__init__(self, id, binDir, binName)
        self.workers = workers
        self.threads = threads
        self.dispatchAlgStr = dispatchAlgStr
        self.runtimeStats = runtimeStats


    def run(self, batchDir, testFile, timeout):
        hyloresCmdLine = ['%s'          % self.binLocation,
                          '-o', '%s'    % self.complStr,
                          '-F', '%s'    % self.selFuncStr,
                          '-S', '%s'    % self.statsStr,
                          '-t', '%i'    % timeout,
                          '-f', '%s/%s' % (batchDir, testFile)]
        if self.workers > 0:
            hyloresCmdLine = hyloresCmdLine + ['-n', '%i'% self.workers,
                                               '-D', '%s'% self.dispatchAlgStr,
                                               '+RTS','-N%i'% self.threads,'-A20m','-RTS']

        if self.workers == 0:
            hyloresCmdLine = hyloresCmdLine + ['+RTS']

        if self.runtimeStats:
            hyloresCmdLine = hyloresCmdLine + ['-s%s.stat'%  testFile]

        self.runWrapper(hyloresCmdLine,
                        testFile,
                        allowedErrorCodes = [1, 2, 3])  # SAT / UNSAT / TIMEOUT


    def newStatistics(self):
        return HyLoResV2_5Statistics(self.workers, self.runtimeStats)


    def getConfiguration(self):
        config = []
        if self.workers > 0:
            version = ("Version", "2.5 - Parallel")
            config  = [("Workers", '%i' % self.workers),
                       ("Threads", '%i' % self.threads),
                       ("Dispatch Algorithm", '%s' % self.dispatchAlgStr)]
        else:
            version = ("Version", "2.5 - Serial")
            config = [("Prover", "hylores"),
                  version,
                  ("Given clause strategy", self.complStr),
                  ("Selection function", self.selFuncStr),
                  ("Statistics generation", self.statsStr)] + config
        return config


class WorkerStat(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.rawClausesGenerated       = [Statistics.INFINITY]
        self.nonFwSubsClausesGenerated = [Statistics.INFINITY]
        self.disRuleCount              = [0]
        self.conRuleCount              = [0]
        self.resPropRuleCount          = [0]
        self.resNomRuleCount           = [0]
        self.resBoxRuleCount           = [0]
        self.boxRuleCount              = [0]
        self.boxXRuleCount             = [0]
        self.diaRuleCount              = [0]
        self.downRuleCount             = [0]
        self.parUnitRuleCount          = [0]
        self.parPropRuleCount          = [0]
        self.parNegPropRuleCount       = [0]
        self.parEqRuleCount            = [0]
        self.parNeqRuleCount           = [0]
        self.parBoxRuleCount           = [0]
        self.parRelRuleCount           = [0]


    def parsers(self):
        return self.phaseTwoWorkers


    def addNewColumn(self):
        self.rawClausesGenerated.append(Statistics.INFINITY)
        self.nonFwSubsClausesGenerated.append(Statistics.INFINITY)
        self.disRuleCount.append(0)
        self.conRuleCount.append(0)
        self.resPropRuleCount.append(0)
        self.resNomRuleCount.append(0)
        self.resBoxRuleCount.append(0)
        self.boxRuleCount.append(0)
        self.boxXRuleCount.append(0)
        self.diaRuleCount.append(0)
        self.downRuleCount.append(0)
        self.parUnitRuleCount.append(0)
        self.parPropRuleCount.append(0)
        self.parNegPropRuleCount.append(0)
        self.parEqRuleCount.append(0)
        self.parNeqRuleCount.append(0)
        self.parBoxRuleCount.append(0)
        self.parRelRuleCount.append(0)

    def getStatValues(self):
	temp = {}
        temp['rawClausesGenerated']       = self.rawClausesGenerated
        temp['nonFwSubsClausesGenerated'] = self.nonFwSubsClausesGenerated
        temp['disRuleCount']              = self.disRuleCount
        temp['conRuleCount']              = self.conRuleCount
        temp['resPropRuleCount']          = self.resPropRuleCount
        temp['resNomRuleCount']           = self.resNomRuleCount
        temp['resBoxRuleCount']           = self.resBoxRuleCount
        temp['boxRuleCount']              = self.boxRuleCount
        temp['boxXRuleCount']             = self.boxXRuleCount
        temp['diaRuleCount']              = self.diaRuleCount
        temp['downRuleCount']             = self.downRuleCount
        temp['parUnitRuleCount']          = self.parUnitRuleCount
        temp['parPropRuleCount']          = self.parPropRuleCount
        temp['parNegPropRuleCount']       = self.parNegPropRuleCount
        temp['parEqRuleCount']            = self.parEqRuleCount
        temp['parNeqRuleCount']           = self.parNeqRuleCount
        temp['parBoxRuleCount']           = self.parBoxRuleCount
        temp['parRelRuleCount']           = self.parRelRuleCount

        return temp

    def setStatValues(self, stats):
        self.rawClausesGenerated       = stats['rawClausesGenerated']
        self.nonFwSubsClausesGenerated = stats['nonFwSubsClausesGenerated']
        self.disRuleCount              = stats['disRuleCount']
        self.conRuleCount              = stats['conRuleCount']
        self.resPropRuleCount          = stats['resPropRuleCount']
        self.resNomRuleCount           = stats['resNomRuleCount']
        self.resBoxRuleCount           = stats['resBoxRuleCount']
        self.boxRuleCount              = stats['boxRuleCount']
        self.boxXRuleCount             = stats['boxXRuleCount']
        self.diaRuleCount              = stats['diaRuleCount']
        self.downRuleCount             = stats['downRuleCount']
        self.parUnitRuleCount          = stats['parUnitRuleCount']
        self.parPropRuleCount          = stats['parPropRuleCount']
        self.parNegPropRuleCount       = stats['parNegPropRuleCount']
        self.parEqRuleCount            = stats['parEqRuleCount']
        self.parNeqRuleCount           = stats['parNeqRuleCount']
        self.parBoxRuleCount           = stats['parBoxRuleCount']
        self.parRelRuleCount           = stats['parRelRuleCount']


    def acceptLegacyClausesGenerated(self, line):
        """ Accepts the 'Clauses generated' line that comes from ver 1.0 """
        return self.accept(r'^Clauses generated: \d+$', '__legacyClausesGeneratedParser', line)


    def parseRawClausesGenerated(self, line):
        return self.__parseMetric(r'^Clauses generated \(raw\): (\d+)$', 'rawClausesGenerated', line)


    def parseNonFwSubsClausesGenerated(self, line):
        return self.__parseMetric(r'^Clauses generated \(non forward-subsumed\): (\d+)$', 'nonFwSubsClausesGenerated', line)


    def parseDisRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_Disj rule: (\d+)$', 'disRuleCount', line)


    def parseConRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_Conj rule: (\d+)$', 'conRuleCount', line)


    def parseResPropRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ResP rule: (\d+)$', 'resPropRuleCount', line)


    def parseResNomRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ResN rule: (\d+)$', 'resNomRuleCount', line)


    def parseResBoxRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ResB rule: (\d+)$', 'resBoxRuleCount', line)


    def parseBoxRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_Box rule: (\d+)$', 'boxRuleCount', line)


    def parseBoxXRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_BoxX rule: (\d+)$', 'boxXRuleCount', line)


    def parseDiaRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_Dia rule: (\d+)$', 'diaRuleCount', line)


    def parseDownRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_Down rule: (\d+)$', 'downRuleCount', line)


    def parseParPropRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ParP rule: (\d+)$', 'parPropRuleCount', line)


    def parseParNegPropRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ParNegP rule: (\d+)$', 'parNegPropRuleCount', line)


    def parseParEqRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ParEq rule: (\d+)$', 'parEqRuleCount', line)


    def parseParNeqRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ParNeq rule: (\d+)$', 'parNeqRuleCount', line)


    def parseParUnitRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ParUnit rule: (\d+)$', 'parUnitRuleCount', line)


    def parseParBoxRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ParB rule: (\d+)$', 'parBoxRuleCount', line)


    def parseParRelRuleCount(self, line):
        return self.__parseMetric(r'^\s*R_ParR rule: (\d+)$', 'parRelRuleCount', line)


    def __parseMetric(self, pattern, metricAttr, line):
        parser  = self.retrieveParser(pattern, '__' + metricAttr + 'Parser')
        matched = parser.search(line)
        if matched:
            val = float(matched.group(1))
            assert(val != None)
            self.replaceLastVal(getattr(self, metricAttr), val)

        return matched != None


    def parseWorker(self, file):
        eof = False
        phaseTwo = [(self.acceptLegacyClausesGenerated,   False),  # Format: (parser, mandatory)
                    (self.parseRawClausesGenerated,       True),
                    (self.parseNonFwSubsClausesGenerated, True),
                    (self.parseDisRuleCount,              False),
                    (self.parseConRuleCount,              False),
                    (self.parseResPropRuleCount,          False),
                    (self.parseResNomRuleCount,           False),
                    (self.parseResBoxRuleCount,           False),
                    (self.parseBoxRuleCount,              False),
                    (self.parseBoxXRuleCount,             False),
                    (self.parseDiaRuleCount,              False),
                    (self.parseDownRuleCount,             False),
                    (self.parseParUnitRuleCount,          False),
                    (self.parseParPropRuleCount,          False),
                    (self.parseParNegPropRuleCount,       False),
                    (self.parseParEqRuleCount,            False),
                    (self.parseParNeqRuleCount,           False),
                    (self.parseParBoxRuleCount,           False),
                    (self.parseParRelRuleCount,           False)]

        while (not eof):
              line = file.readline()
              if line == "" or line == 'end\n':
                 break  # is there a better way of doing this?

              if not self.isUselessLine(line):
                 parsed = False
                 for (parser, mandatory) in phaseTwo:
                     parsed = parser(line)
                     if parsed:
                        phaseTwo.remove((parser, mandatory))
                        break
                 else:
                      sys.exit("Error: Unparseable line '%s' in file '%s'" % (line, file))

        # every mandatory parser must have been consumed
        remainingMandatoryParsers = filter(lambda (parser, mandatory): mandatory, phaseTwo)
        if ( len(remainingMandatoryParsers) > 0 ):
           sys.exit("Error: File '%s' had no line parseable by '%s'" % (file, remainingMandatoryParsers[0]))


    def isUselessLine(self, line):
        """ Accepts all lines with no relevant statistical information """
        return self.accept(r'^-*$|^File \.hyloresrc does not exists.$|'\
                           r'^ *\[.*\],?$|'\
                           r'^Writing default configuration file.$|^\(final statistics\)$|'\
                           r'^begin$|^Rule applications:$|^Elapsed time:',
                           '__uselessLinesParser', line)


class HyLoResV2_5Statistics(Statistics):
    def __init__(self, workers, runtimeStats):
        Statistics.__init__(self)
        self.workerStats = []
        self.runtimeStats = runtimeStats
        if runtimeStats:
            self.GCpercentage = []
        if workers == 0:
            workers = 1

        for i in range(workers):
            self.workerStats.append(WorkerStat())

        # In phase one, all the parsers are mandatory
        self.phaseOneParsers = [self.acceptInputFormulaHeader,
                                self.acceptInputFormulaFooter,
                                self.parseSatUnsatTimeout]


    def addNewColumn(self):
        Statistics.addNewColumn(self)
        if self.runtimeStats:
           self.GCpercentage.append(0)
        for ws in self.workerStats:
            ws.addNewColumn

    def parseGCPercentage(self, line):
        parser = self.retrieveParser(r'^\s*\%GC time\s*\d+\.\d+\%\s*\((\d+\.\d+)\% elapsed\)$',
                                      '__GCPercentageParser')
        matched = parser.search(line)
        if matched:
            val = float(matched.group(1))
            assert(val != None)
            self.replaceLastVal(getattr(self, 'GCpercentage'), val)

        return matched != None

    def parseStats(self, statFile):
        if (os.path.exists(statFile)):
            try:
                f = open(statFile)
                eof = False
                while (not eof):
                    line = f.readline()
                    eof  = line == ""
                    if (not eof):
                        self.parseGCPercentage(line)
                f.close()
            except:
                print "Error parsing '%s'" % statFile
                raise

    def getStatValues(self):
        temp = Statistics.getStatValues(self)
        temp["workerStats"] = []
        for ws in self.workerStats:
            temp["workerStats"].append(ws.getStatValues())
        temp["runtimeStats"] = None # fix this if you need them
        return temp

    def setStatValues(self, stats):
        Statistics.setStatValues(self,stats)
        self.workerStats = []
        for ws in stats["workerStats"]:
          wStat = WorkerStat()
          wStat.setStatValues(ws)
          self.workerStats.append(wStat)
        self.runtimeStats = stats["runtimeStats"]
        self.workers = len(self.workerStats)

    def consolidate(self, stats):
        all_lists = self.getStatValues().keys()
        all_lists.remove("workerStats")
        all_lists.remove("runtimeStats")
        Statistics.consolidateTheseLists(self, stats, all_lists)
        for i in range(len(self.workerStats)):
            self.workerStats[i].consolidate(stats.workerStats[i])

    def parseResponse(self, hlrResponseFile):
        try:
            f        = open(hlrResponseFile)
            phaseOne = self.phaseOneParsers[:]
            eof      = False
            while (not eof) and len(phaseOne) > 0:
                line = f.readline()
                eof  = line == ""
                if (not eof) and (not self.isUselessLine(line)):
                    parsed = False
                    for parser in phaseOne:
                        parsed = parser(line)
                        if parsed:
                            phaseOne.remove(parser)
                            break
                    else:
                        sys.exit("Error unparseable line (phase 1) '%s' in file '%s'" % (line, hlrResponseFile))

            # If there was no timeout, statistics follow....
            lastAnswer = self.answer[-1]
            assert(lastAnswer != None)

            for ws in self.workerStats:
                ws.parseWorker(f)

            f.close()

        except:
            print "Error parsing '%s'" % hlrResponseFile
            raise


    def isUselessLine(self, line):
        """ Accepts all lines with no relevant statistical information """
        return self.accept(r'^-*$|^File \.hyloresrc does not exists.$|'\
                           r'^ *\[.*\],?$|'\
                           r'^Writing default configuration file.$|^\(final statistics\)$|'\
                           r'^begin$|^Rule applications:$|^Elapsed time:',
                           '__uselessLinesParser', line)


    def acceptInputFormulaHeader(self, line):
        """ Accepts the 'Input:' that preceeds the input formula """
        return self.accept(r'^Input:$', '__inputFormulaHeaderParser', line)


    def acceptInputFormula(self, line):
        """ Accepts the input formula """
        return self.accept(r'^   \[.*\]$', '__inputFormulaParser', line)


    def acceptInputFormulaFooter(self, line):
        """ Accepts the 'End of input' that cames after the input formula """
        return self.accept(r'^End of input$', '__inputFormulaFooterParser', line)


    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser  = self.retrieveParser(r'^The formula is (satisfiable)$|^The formula is (unsatisfiable)$|^(Timeout) reached.$',
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

        except NoParse:
            # hylores normally ends with code 1, 2 or 3, meaning
            # sat, unsat or timeout. some implementations of time
            # might print an error message in this case
            self._parseTimeSkipping(1, timeResponseFile)


    def clausesGeneratedPerSecond(self):
        return self.summarizeAttr('rawClausesGenerated') / self.sumUserTime

    def rawClausesGenerated(self):
        return self.summarizeAttr('rawClausesGenerated')

    def nonFwSubsClausesGenerated(self):
        return self.summarizeAttr('nonFwSubsClausesGenerated')

    def disRuleCount(self):
        return self.summarizeAttr('disRuleCount')

    def conRuleCount(self):
        return self.summarizeAttr('conRuleCount')

    def resPropRuleCount(self):
        return self.summarizeAttr('resPropRuleCount')

    def resNomRuleCount(self):
        return self.summarizeAttr('resNomRuleCount')

    def resBoxRuleCount(self):
        return self.summarizeAttr('resBoxRuleCount')

    def boxRuleCount(self):
        return self.summarizeAttr('boxRuleCount')

    def diaRuleCount(self):
        return self.summarizeAttr('diaRuleCount')

    def downRuleCount(self):
        return self.summarizeAttr('downRuleCount')

    def parUnitRuleCount(self):
        return self.summarizeAttr('parUnitRuleCount')

    def parPropRuleCount(self):
        return self.summarizeAttr('parPropRuleCount')

    def parNegPropRuleCount(self):
        return self.summarizeAttr('parNegPropRuleCount')

    def parNeqRuleCount(self):
        return self.summarizeAttr('parNeqRuleCount')

    def parBoxRuleCount(self):
        return self.summarizeAttr('parBoxRuleCount')

    def parRelRuleCount(self):
        return self.summarizeAttr('parRelRuleCount')

    def parEqRuleCount(self):
        return self.summarizeAttr('parEqRuleCount')


    def sumUserTime(self):
        result = 0
        for i in range(len(self.usertime)):
            result += usertime [i]
        return result


    def summarizeAttr(self,attrName):
        result = []
        attr   = getattr(self.workerStats[-1],attrName)
        for i in range(len(attr)):
            result.append(0)
            for ws in self.workerStats:
                result[i] += getattr(ws,attrName) [i]
        return result
