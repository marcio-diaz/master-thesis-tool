# -*- coding: iso-8859-1 -*-
""" HyLoResV2 module
"""


import re

from prover        import Prover, Statistics
from miscFunctions import *


class HyLoResV2(Prover):
    def __init__(self, id, binDir, binName = 'hylores'):
        Prover.__init__(self,id)
        self.binLocation = '%s/%s' % (binDir, binName)
        self.complStr    = 'sDMBLV'
        self.selFuncStr  = 'dboan'
        self.statsStr    = 'rgG:'


    def requiredBinaries(self):
        return [self.binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        timeArgs        = ['-p']
        timeResponse    = self.timeResponse(testFile)
        hyloresCmdLine  = ['%s'           % self.binLocation,
                           '-o',  '%s'    % self.complStr,
                           '-sf', '%s'    % self.selFuncStr,
                           '-st', '%s'    % self.statsStr,
                           '-t',  '%i'    % timeout,
                           '-f',  '%s/%s' % (batchDir, testFile)]
        hyloresResponse =  '%s' % self.proverResponse(testFile)

        systemOrDie("time", timeArgs + hyloresCmdLine, stdout = hyloresResponse, stderr = timeResponse)
        removeOrDie(".hyloresrc");


    def newStatistics(self):
       return HyLoResV2Statistics()


    def getConfiguration(self):
        return  [("Prover", "hylores"),
                 ("Version", "2.0 (haskell monads)"),
                 ("Given clause strategy", self.complStr),
                 ("Selection function", self.selFuncStr),
                 ("Statistics generation", self.statsStr)]


class HyLoResV2Statistics(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.rawClausesGenerated       = []
        self.nonFwSubsClausesGenerated = []
        self.disRuleCount              = []
        self.conRuleCount              = []
        self.resRuleCount              = []
        self.boxRuleCount              = []
        self.boxResRuleCount           = []
        self.diaRuleCount              = []
        self.parRuleCount              = []
        self.parRelRuleCount           = []
        # In phase one, all the parsers are mandatory
        self.phaseOneParsers = [self.acceptInputFormulaHeader, self.acceptInputFormula,
                                self.acceptInputFormulaFooter, self.parseSatUnsatTimeout]
        # In phase two, some information might not come
        self.phaseTwoParsers = [(self.acceptLegacyClausesGenerated,   False),  # Format: (parser, mandatory)
                                (self.parseRawClausesGenerated,       True),
                                (self.parseNonFwSubsClausesGenerated, True),
                                (self.parseDisRuleCount,              False),
                                (self.parseConRuleCount,              False),
                                (self.parseResRuleCount,              False),
                                (self.parseBoxRuleCount,              False),
                                (self.parseBoxResRuleCount,           False),
                                (self.parseDiaRuleCount,              False),
                                (self.parseParRuleCount,              False),
                                (self.parseParRelRuleCount,           False)]


    def addNewColumn(self):
        Statistics.addNewColumn(self)
        self.rawClausesGenerated.append(Statistics.INFINITY)
        self.nonFwSubsClausesGenerated.append(Statistics.INFINITY)
        self.disRuleCount.append(0)
        self.conRuleCount.append(0)
        self.resRuleCount.append(0)
        self.boxRuleCount.append(0)
        self.boxResRuleCount.append(0)
        self.diaRuleCount.append(0)
        self.parRuleCount.append(0)
        self.parRelRuleCount.append(0)

    def getStatValues(self):
        temp = Statistics.getStatValues(self)
        temp['rawClausesGenerated']       = self.rawClausesGenerated
        temp['nonFwSubsClausesGenerated'] = self.nonFwSubsClausesGenerated
        temp['disRuleCount']              = self.disRuleCount
        temp['conRuleCount']              = self.conRuleCount
        temp['resRuleCount']              = self.resRuleCount
        temp['boxRuleCount']              = self.boxRuleCount
        temp['boxResRuleCount']           = self.boxResRuleCount
        temp['diaRuleCount']              = self.diaRuleCount
        temp['parRuleCount']              = self.parRuleCount
        temp['parRelRuleCount']           = self.parRelRuleCount

        return temp

    def setStatValues(self, stats):
        Statistics.setStatValues(self, stats)
        self.rawClausesGenerated       = stats['rawClausesGenerated']
        self.nonFwSubsClausesGenerated = stats['nonFwSubsClausesGenerated']
        self.disRuleCount              = stats['disRuleCount']
        self.conRuleCount              = stats['conRuleCount']
        self.resRuleCount              = stats['resRuleCount']
        self.boxRuleCount              = stats['boxRuleCount']
        self.boxResRuleCount           = stats['boxResRuleCount']
        self.diaRuleCount              = stats['diaRuleCount']
        self.parRuleCount              = stats['parRuleCount']
        self.parRelRuleCount           = stats['parRelRuleCount']


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
            if lastAnswer != Statistics.ANSWER_TIMEOUT:
                phaseTwo = self.phaseTwoParsers[:]
                while True:
                    line = f.readline()
                    if line == "":
                        break    # is there a better way of doing this?

                    if not self.isUselessLine(line):
                        parsed = False
                        for (parser, mandatory) in phaseTwo:
                            parsed = parser(line)
                            if parsed:
                                phaseTwo.remove((parser, mandatory))
                                break
                        else:
                            sys.exit("Error: Unparseable line '%s' in file '%s'" % (line, hlrResponseFile))

                # every mandatory parser must have been consumed
                remainingMandatoryParsers = filter(lambda (parser, mandatory): mandatory, phaseTwo)
                if ( len(remainingMandatoryParsers) > 0 ):
                    print "Error: File '%s' had no line parseable by '%s'" % (hlrResponseFile, remainingMandatoryParsers[0])
                    sys.exit(1)

            f.close()

        except:
            print "Error parsing '%s'" % hlrResponseFile
            raise


    def isUselessLine(self, line):
        """ Accepts all lines with no relevant statistical information """
        return self.accept(r'^-*$|^File \.hyloresrc does not exists.$|'\
                           r'^Writing default configuration file.$|^\(final statistics\)$|'\
                           r'^begin$|^end$|^Rule applications:$|^Elapsed time:',
                           '__uselessLinesParser', line)


    def acceptInputFormulaHeader(self, line):
        """ Accepts the 'Input:' that preceeds the input formula """
        return self.accept(r'^Input:$', '__inputFormulaHeaderParser', line)


    def acceptInputFormula(self, line):
        """ Accepts the input formula """
        return self.accept(r'^ {.*}$', '__inputFormulaParser', line)


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


    def acceptLegacyClausesGenerated(self, line):
        """ Accepts the 'Clauses generated' line that comes from ver 1.0 """
        return self.accept(r'^Clauses generated: \d+$', '__legacyClausesGeneratedParser', line)


    def parseRawClausesGenerated(self, line):
        return self.__parseMetric(r'^Clauses generated \(raw\): (\d+)$', 'rawClausesGenerated', line)


    def parseNonFwSubsClausesGenerated(self, line):
        return self.__parseMetric(r'^Clauses generated \(non forward-subsumed\): (\d+)$', 'nonFwSubsClausesGenerated', line)


    def parseDisRuleCount(self, line):
        return self.__parseMetric(r'^\s*DIS rule: (\d+)$', 'disRuleCount', line)


    def parseConRuleCount(self, line):
        return self.__parseMetric(r'^\s*CON rule: (\d+)$', 'conRuleCount', line)


    def parseResRuleCount(self, line):
        return self.__parseMetric(r'^\s*RES rule: (\d+)$', 'resRuleCount', line)


    def parseBoxRuleCount(self, line):
        return self.__parseMetric(r'^\s*BOX rule: (\d+)$', 'boxRuleCount', line)


    def parseBoxResRuleCount(self, line):
        return self.__parseMetric(r'^\s*BOXRES rule: (\d+)$', 'boxResRuleCount', line)


    def parseDiaRuleCount(self, line):
        return self.__parseMetric(r'^\s*DIA rule: (\d+)$', 'diaRuleCount', line)


    def parseParRuleCount(self, line):
        return self.__parseMetric(r'^\s*PARAM rule: (\d+)$', 'parRuleCount', line)


    def parseParRelRuleCount(self, line):
        return self.__parseMetric(r'^\s*PARAMREL rule: (\d+)$', 'parRelRuleCount', line)


    def __parseMetric(self, pattern, metricAttr, line):
        parser = self.retrieveParser(pattern, '__' + metricAttr + 'Parser')
        matched = parser.search(line)
        if matched:
            val = float(matched.group(1))
            assert(val != None)
            self.replaceLastVal(getattr(self, metricAttr), val)

        return matched != None

