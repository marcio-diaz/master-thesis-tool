import re

from prover        import Prover, Statistics, NoParse
from miscFunctions import *


class Racer_conv(Prover):
    def __init__(self, id, binDir, binName = 'convRacer'):
        Prover.__init__(self,id)
        self.__binLocation = '%s/%s' % (binDir, binName)


    def requiredBinaries(self):
        return [self.__binLocation]


    def requiredFiles(self):
        return []


    def run(self, batchDir, testFile, timeout):
        racerCmdLine  = ['%s'    % self.__binLocation,
                         '%s/%s' % (batchDir, testFile),
                         '%i'    % timeout]

        self.runWrapper(racerCmdLine, testFile)


    def newStatistics(self):
       return RacerStatistics()


    def getConfiguration(self):
        return  [("Prover", "RacerPro"),
                 ("Version", '1.9.2beta')]


class RacerStatistics(Statistics):
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
                        phaseOne[idx:idx+1] = []
                    else:
                        print "Error unparseable line (phase 1) '%s' in file '%s'" % (line, htabResponseFile)
                        sys.exit(1)

            f.close()

        except:
            print "Error parsing '%s'" % htabResponseFile
            raise


    def isUselessLine(self, line):
        """ Accepts all lines with no relevant statistical information """
        return self.accept(r'^\(in\-tbox.*$|'\
                           r'^$|'\
                           r'^.*causes a cycle in TBox translated.$|'\
                           r'^;;;.*$',
                           '__uselessLinesParser', line)


    def parseSatUnsatTimeout(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        parser = self.retrieveParser(r'^(.*\(concept\-satisfiable.*t)$|'\
                                     r'^(.*\(concept\-satisfiable.*nil)$|'\
                                     r'^(TIMEOUT).*$',
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

