""" HTab module
"""


import re

from prover        import Prover, Statistics
from miscFunctions import *





class HTab(Prover):
  def __init__(self, id, binDir, binName = 'htab', extraArgs=''):
    Prover.__init__(self, id)
    self.binLocation = '%s/%s' % (binDir, binName)
    self.statsStr    = ':0:c'
    self.extraArgs   = extraArgs.split()


  def requiredBinaries(self):
    return [self.binLocation]


  def requiredFiles(self):
    return []


  def run(self, batchDir, testFile, timeout):
    cmdLine  = ['%s'        % self.binLocation,
                '-t',  '%i'    % timeout,
                '-f',  '%s/%s' % (batchDir, testFile)]  + self.extraArgs

    self.runWrapper(cmdLine, 
                    testFile,
                    allowedErrorCodes=[1,2,3,256,512,768]) # the last 3 numbers are a bash hack


  def newStatistics(self):
    return HTabStatistics()


  def getConfiguration(self):
    return  [("Prover", "htab"),
             ("Version", "1.5.4")]


class HTabStatistics(Statistics):
  def __init__(self):
    Statistics.__init__(self)
    self.phaseOneParsers = [self.parseSatUnsatTimeout,
                            self.parseClosedBranchesCount]

    self.closedBranchesCount       = []
    self.mergeRuleCount            = []


  def addNewColumn(self):
    Statistics.addNewColumn(self)
    self.closedBranchesCount.append(0)


  def getStatValues(self):
    temp = Statistics.getStatValues(self)
    temp['closedBranchesCount'] = self.closedBranchesCount

    return temp


  def setStatValues(self, stats):
    Statistics.setStatValues(self, stats)
    self.closedBranchesCount = stats['closedBranchesCount']


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
    return self.accept(r'^File .*\.htabrc does not exist.$|'\
                       r'^== Checking theory satisfiability ==$|'\
                       r'^\* Satisfiability task$|'\
                       r'^Input:|'\
                       r'^Elapsed time:.*|'\
                       r'^Input for SAT test:$|'\
                       r'^{.*}$|'\
                       r'^End of input$|'\
                       r'^Writing default configuration file.$|'\
                       r'^Reading parameters from .htabrc$|'\
                       r'^\(final statistics\)$|'\
                       r'^begin$|'\
                       r'^end$|'\
                       r'^\-*$|'\
                       r'^Rule applications:$|'\
                       r'Task time:.*$|'\
                       r'^All tasks successful.|One task failed.$|'\
                       r'Total time:.*$',
                       '__uselessLinesParser', line)


  def parseSatUnsatTimeout(self, line):
    """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
    parser  = self.retrieveParser(r'^(The formula is satisfiable.)$|'\
                                 r'^(The formula is unsatisfiable.)$|'\
                                 r'^(Timeout.)$',
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


  def parseClosedBranchesCount(self, line):
    return self.__parseMetric(r'^Closed branches: (\d+)$', 'closedBranchesCount', line)


  def __parseMetric(self, pattern, metricAttr, line):
    parser = self.retrieveParser(pattern, '__' + metricAttr + 'Parser')
    matched = parser.search(line)
    if matched:
      val = float(matched.group(1))
      assert(val != None)
      self.replaceLastVal(getattr(self, metricAttr), val)

    return matched != None

