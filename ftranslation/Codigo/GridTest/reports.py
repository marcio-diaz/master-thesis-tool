# -*- coding: utf-8 -*-
""" reports modules
"""


from miscFunctions import *

# this line extends the modules search path in order to find the prover's drivers in the "drivers" directory
import sys
sys.path.append(fullabsexpand('drivers'))

import os
# import platform
import copy

from prover        import Statistics
from gnuplotPlots  import generatePlot, generateMaxPlot, generateMedianPlot, generateBoxPlot, defaultPlotExt


class Report:
  def __init__(self, hcnfParams, currentBatch = None):
    self.hcnfParams = hcnfParams

    if currentBatch:
      self.numOfAvailBatches = currentBatch
    else:
      self.numOfAvailBatches = len(hcnfParams.batches)

    self.__latexSections  = []
    self.__gnuplotScripts = []
    self.__gnuplotBin     = which('gnuplot')
    self.__latexBin       = which('latex')
    self.__dvipsBin       = which('dvips')


  def generateFullReport(self):
    self.generateTestDescription()
    self.generateSatUnsatPlots()
    self.generateTimePlots()
    self.generateGCPercentagePlot()
    self.generateClausesPlot()
    # todo: fixme!!!
    # self.generateClausesPerSecondPlot()
    self.generateWorkersLoadPlot()
    self.generateResponsesAnalysis()
    self.generateUniqueResponsesPlot()
    self.generateLatexDocument()


  def canCompile(self):
    return self.__gnuplotBin and self.__latexBin and self.__dvipsBin


  def compile(self):
    for gnuplotScript in self.__gnuplotScripts:
      systemOrDie(self.__gnuplotBin, [gnuplotScript], stdout = None)
      

    docPrefix = self.__mkFileName('results')
    for i in range(3):
      systemOrDie(self.__latexBin, ['-interaction=nonstopmode', docPrefix + '.tex'], stdout = None)


    systemOrDie(self.__dvipsBin, ['-o', '%s.ps' % docPrefix, '%s.dvi' % docPrefix], stdout = None, stderr = None)

  def generateLatexDocument(self):
    testId = self.hcnfParams.testId
    f = open('%s_results.tex' % testId, 'w')
    f.write(r'\documentclass{article}' '\n\n')
#    f.write(r'\usepackage{a4wide}' '\n')
    f.write(r'\usepackage{fullpage}' '\n')
    f.write(r'\usepackage{mdwlist}' '\n')
    f.write(r'\usepackage{graphicx}' '\n\n')
    f.write(r'\title{%s\\Test results}' '\n\n' % self.__latexEsc(testId))
    f.write(r'\begin{document}' '\n')
    for section in self.__latexSections:
        f.write(r'\include{%s}' '\n' % section)

    f.write(r'\end{document}' '\n')
    f.close()


  def generateTestDescription(self):
    try:
      name = self.__mkFileName('description')
      self.__addSection(name)
      f = open('%s.tex' % name, 'w')
      f.write(r'\section{Test Description}' '\n\n')
      f.write(r'\subsection{Platform}' '\n\n')

      for idx in range(len(self.hcnfParams.system)):
        if self.hcnfParams.system[idx]    == '': self.hcnfParams.system[idx]    = r'\ensuremath{\langle}unknown\ensuremath{\rangle}'
        if self.hcnfParams.host[idx]      == '': self.hcnfParams.host[idx]      = r'\ensuremath{\langle}unknown\ensuremath{\rangle}'
        if self.hcnfParams.release[idx]   == '': self.hcnfParams.release[idx]   = r'\ensuremath{\langle}unknown\ensuremath{\rangle}'
        if self.hcnfParams.version[idx]   == '': self.hcnfParams.version[idx]   = r'\ensuremath{\langle}unknown\ensuremath{\rangle}'
        if self.hcnfParams.machine[idx]   == '': self.hcnfParams.machine[idx]   = r'\ensuremath{\langle}unknown\ensuremath{\rangle}'
        if self.hcnfParams.processor[idx] == '': self.hcnfParams.processor[idx] = r'\ensuremath{\langle}unknown\ensuremath{\rangle}'

        f.write(r'\subsubsection{%s}' '\n\n' % self.__latexEsc(self.hcnfParams.host[idx]))
        f.write(r'  \begin{description*}' '\n')
        f.write(r'    \item[Architecure] %s' '\n' % self.__latexEsc(self.hcnfParams.machine[idx]))
        f.write(r'    \item[Processor] %s' '\n' % self.__latexEsc(self.hcnfParams.processor[idx]))
        f.write(r'    \item[Operating system] %s %s (%s)' '\n' % (self.__latexEsc(self.hcnfParams.system[idx]), self.__latexEsc(self.hcnfParams.release[idx]), self.__latexEsc(self.hcnfParams.version[idx])))
        f.write(r'  \end{description*}' '\n\n')

      f.write(r'\subsection{Parameters}' '\n\n')
      f.write(r'\begin{description*}' '\n')
      f.write(r'  \item[Test id] %s' '\n' % self.__latexEsc(self.hcnfParams.testId))
      f.write(r'  \item[Propositional symbols (\#, freq)] (%i, %i)' '\n' % (self.hcnfParams.numOfProps, self.hcnfParams.freqOfProps))
      f.write(r'  \item[Nominals (\#, freq)] (%i, %i)' '\n' % (self.hcnfParams.numOfNoms, self.hcnfParams.freqOfNoms))
      f.write(r'  \item[State variables (\#, freq)] (%i, %i)' '\n' % (self.hcnfParams.numOfSVars, self.hcnfParams. freqOfSVars))
      f.write(r'  \item[Relations] %i' '\n' % self.hcnfParams.numOfRels)
      f.write(r'  \item[Max. (global) depth] %i' '\n' % self.hcnfParams.maxDepth)
      f.write(r'  \item[Force depth] %s' '\n' % self.hcnfParams.forceDepth)
      f.write(r'  \item[Diamonds (depth, freq)] (%i, %i)' '\n' % (self.hcnfParams.diamDepth, self.hcnfParams.freqOfDiam))
      f.write(r'  \item[At (depth, freq)] (%i, %i)' '\n' % (self.hcnfParams.atDepth, self.hcnfParams.freqOfAt))
      f.write(r'  \item[Downarrow (depth, freq)] (%i, %i)' '\n' % (self.hcnfParams.downDepth, self.hcnfParams.freqOfDown))
      f.write(r'  \item[Inverse modality (depth, freq)] (%i, %i)' '\n' % (self.hcnfParams.invDepth, self.hcnfParams.freqOfInv))
      f.write(r'  \item[Universal modality (depth, freq)] (%i, %i)' '\n' % (self.hcnfParams.univDepth, self.hcnfParams.freqOfUniv))
      f.write(r'  \item[Difference modality (depth, freq)] (%i, %i)' '\n' % (self.hcnfParams.diffDepth, self.hcnfParams.freqOfDiff))
      f.write(r'  \item[Clause size distribution] %s' '\n' % self.hcnfParams.clauseSizeDistrib)
      f.write(r'  \item[Probability of a modal operator] %i' '\n' % self.hcnfParams.probOperator)
      f.write(r'  \item[Probability of negating a modal operator] %i' '\n' % self.hcnfParams.probNegation)
      f.write(r'  \item[Batch size] %i' '\n' % self.hcnfParams.batchSize)
      f.write(r'  \item[Range of clauses] [%i\ldots%i]' '\n' % (self.hcnfParams.fromNumClauses, self.hcnfParams.toNumClauses))
      f.write(r'  \item[Step] %i' '\n' % self.hcnfParams.step)
      f.write(r'  \item[Timeout] %i seconds' '\n' % self.hcnfParams.timeout)
      f.write(r'\end{description*}' '\n\n')

      f.write(r'\subsection{Provers}' '\n\n')
      for prover in self.hcnfParams.provers:
        f.write(r'\subsubsection{%s}' '\n\n' % self.__latexEsc(prover.id))
        f.write(r'\begin{description*}' '\n')
        for (key, value) in prover.getConfiguration():
          f.write(r'\item[%s] %s' '\n' % (self.__latexEsc(key), self.__latexEsc(value)))

        f.write(r'\end{description*}' '\n\n')

      f.close()

    except:
      print('Error generating test description')
      raise


  def generateResponsesAnalysis(self):
    try:
      name = self.__mkFileName('resp_analysis')
      self.__addSection(name)
      f = open('%s.tex' % name, 'w')
      f.write(r"\section{Analysis of the provers' responses}" '\n\n')
      f.write(r'\subsection{Inconsistent answers}' '\n\n')
      inconsistencies = self.__getInconsistentAnswers()
      if len(inconsistencies) > 0:
        f.write(r'\begin{itemize*}' '\n')
        for (batch, index) in inconsistencies:
          path      = self.hcnfParams.dirStructure.testDirForBatch(batch)
          #ale sort returns null
          testFiles = os.listdir(path)
          testFiles.sort()
          file      = testFiles[index]
          f.write(r'\item Batch %i -- %s:' '\n\n' % (batch, self.__latexEsc(file)))
          f.write(r'\begin{tabular}{l|l}' '\n')
          f.write(r'Prover & Response\\' '\n')
          f.write(r'\hline\\' '\n')
          for prover in self.hcnfParams.provers:
            f.write(r'%s & %s\\' '\n' % (self.__latexEsc(prover.id),
                                         self.__answerCodeToHuman(prover.proverStatsForBatch(batch).answer[index])))

          f.write(r'\end{tabular}' '\n')

        f.write(r'\end{itemize*}' '\n')

      else:
        f.write('None detected.\n\n')

      f.close()

    except:
      print('Error generating analysis of responses!')
      raise


  def generateSatUnsatPlots(self):
    try:
      plots = []
      for prover in self.hcnfParams.provers:
        plot = self.generateSatUnsatPlot(prover)
        plots.append(( plot, 'Satisifiability portion for %s' % self.__latexEsc(prover.id)))

      self.__generateLatexFiguresSection(
          fileName           = self.__mkFileName('satunsat'),
          title              = 'Satisfiability fraction',
          figuresAndCaptions = plots)

    except:
      print('Error generating sat/unsat plots!')
      raise


  def generateSatUnsatPlot(self, prover):
    try:
      scriptName = self.__mkFileName('satunsat', prover.id)
      batchSize  = self.hcnfParams.batchSize
      sortedKeys = prover.sortedStatsKeys()

      sats       = map(lambda k: len(filter(lambda x: x == Statistics.ANSWER_SAT, prover.proverStatsForBatch(k).answer)), sortedKeys)
      unsats     = map(lambda k: len(filter(lambda x: x == Statistics.ANSWER_UNSAT, prover.proverStatsForBatch(k).answer)), sortedKeys)
      timeouts   = map(lambda k: len(filter(lambda x: x == Statistics.ANSWER_TIMEOUT, prover.proverStatsForBatch(k).answer)), sortedKeys)

      plotOutput = scriptName
      divSize    = lambda v: float(v)/float(batchSize)

      generatePlot(output     = scriptName,
                   xdata      = self.__availableBatches(),
                   series     = [('sat', map(divSize, sats)), ('unsat', map(divSize, unsats)), ('timeout', map(divSize, timeouts))],
                   title      = 'Sat/Unsat relation of %s with %s' % (self.__gnuplotEsc(prover.id), self.__testDescription()),
                   xlabel     = 'Number of clauses',
                   ylabel     = 'Satisfiability fraction',
                   plotOutput = plotOutput,
                   xrange     = self.__rangeForClauses(),
                   maxY       = 1)
      self.__newGnuplotScript(scriptName)

      return plotOutput

    except:
      print "Error generating sat-unsat plot for '%s'" % prover.id
      raise


  def generateTimePlots(self):
    try:
      plots = []
      for attrName in ["usertime","realtime"]:
        plot = self.generateTimePlot(attrName)
        plots.append(( plot, 'Timings - %s' % self.__latexEsc(attrName)))

      self.__generateLatexFiguresSection(
          fileName           = self.__mkFileName('time'),
          title              = 'Timings',
          figuresAndCaptions = plots)

    except:
      print('Error generating sat/unsat plots!')
      raise


  def generateTimePlot(self, attrName):
    try:
      name = self.__mkFileName('time', attrName)

      series = []
      for prover in self.hcnfParams.provers:
        seriesForProver = self.__extractSeriesFromStatistics(prover, attrName)
        series.extend(seriesForProver)

      plotOutput = name
      generateMedianPlot(output     = name,
                         plotOutput = name,
                         xdata      = self.__availableBatches(),
                         series     = series,
                         title      = '%s with %s' % (attrName, self.__testDescription()),
                         xlabel     = 'Number of clauses',
                         xrange     = self.__rangeForClauses(),
                         ylabel     = 'Median execution time (s)',
                         ylogscale  = True,
                         withErrors = True,
                         keyPos     = 'bmargin right horizontal')

      self.__newGnuplotScript('%s' % name)
      return plotOutput

    except:
      print "Error generating time plot"
      raise


  def generateClausesPlot(self):
    try:
      name = self.__mkFileName('clauses')
      self.__generateMedianPlotFromAttributes(output     = name,
                                              plotOutput = name,
                                              statsAttrs = [('rawClausesGenerated', "raw"),
                                                            ('nonFwSubsClausesGenerated', "distinct")],
                                              xlabel     = 'Number of clauses',
                                              ylabel     = 'Median number of clauses generated',
                                              ylogscale  = True,
                                              keyPos     = 'bmargin right horizontal')


      self.__generateLatexFiguresSection(fileName           = name,
                                         title              = 'Clause generation',
                                         figuresAndCaptions = [(name, 'Compared clause generation')])


      self.__newGnuplotScript('%s' % name)


    except NoAttributeFound:
      print "No info for clauses plot"

    except:
      print "Error generating clauses plot"
      raise


  def generateClausesPerSecondPlot(self):
    try:
      series = []

      for prover in self.hcnfParams.provers:
        proverStats = prover.newStatistics()
        if hasattr(proverStats, 'rawClausesGenerated') and hasattr(proverStats, 'realtime'):
          clausesSeriesForProver = self.__extractSeriesValuesFromStatistics(prover, 'rawClausesGenerated')
          timeSeriesForProver = self.__extractSeriesValuesFromStatistics(prover, 'realtime')

          series.append((self.__gnuplotEsc(prover.id), self.__divSeries(clausesSeriesForProver, timeSeriesForProver)))

      if len(series) == 0:
        raise NoAttributeFound

      name   = self.__mkFileName('clauses_second')
      generateMedianPlot(output     = name,
                         plotOutput = name,
                         xdata      = self.__availableBatches(),
                         series     = series,
                         title      = 'Test with %s' % self.__testDescription(),
                         xlabel     = 'Number of clauses',
                         xrange     = self.__rangeForClauses(),
                         ylabel     = 'Clauses generated per second',
                         ylogscale  = True,
                         keyPos     = 'bmargin right horizontal')

      self.__generateLatexFiguresSection(fileName           = name,
                                         title              = 'Clauses Generated per Second' ,
                                         figuresAndCaptions = [(name, 'Compared clauses generated per second')])

      self.__newGnuplotScript('%s' % name)

    except NoAttributeFound:
      print "No info for clauses per second plot"

    except:
      print "Error generating clauses per second plot"
      raise


  def generateGCPercentagePlot(self):
    try:
      name = self.__mkFileName('gc_percentage')
      self.__generateMedianPlotFromAttributes(output        = name,
                                              plotOutput    = name,
                                              statsAttrs    = [('GCpercentage', "%gc")],
                                              xlabel        = 'Number of clauses',
                                              ylabel        = 'Median % of GC time',
                                              ylogscale     = False,
                                              keyPos        = 'right bottom')

      self.__generateLatexFiguresSection(fileName           = name,
                                         title              = '%GC time',
                                         figuresAndCaptions = [(name, 'Total rules count')])
      self.__newGnuplotScript('%s' % name)

    except NoAttributeFound:
      print "No info for gc percentageplot"

    except:
      print "Error generating rules plot"
      raise


  def generateWorkersLoadPlot(self):
    for prover in self.hcnfParams.provers:
      if hasattr(prover, 'workers') and prover.workers > 1:
        self._generateWorkersLoadPlot(prover)


  def _generateWorkersLoadPlot(self, prover):
    try:
      name         = '%s_workers_load' % prover.id
      series       = []
      workersCount = len(prover.proverStatsForBatch(1).workerStats)

      for w in range (0,workersCount):
        resultForWorker = []
        series.append(('Worker %s' % w, resultForWorker))

      for batch in range(1,self.numOfAvailBatches + 1):
        proverStats = prover.proverStatsForBatch(batch)
        rawClauses  = proverStats.rawClausesGenerated ()
        tot         = sum(rawClauses)
        for i in range (0, workersCount):
          generated = []
          for j in range(0,len(rawClauses)):
            max = -1, -1
            for k in range(0, workersCount):
              wsStat = proverStats.workerStats[k]
              gen    = wsStat.rawClausesGenerated[j]
              if gen > max[1]:
                max = k, gen

            generated.append(max[1])
            proverStats.workerStats[max[0]].rawClausesGenerated[j] = -1

          percent = sum(generated) / tot * 100
          series[i][1].append(percent)
          self.__newGnuplotScript('%s' % name)

      generateBoxPlot(output     = name,
                      plotOutput = name,
                      series     = series,
                      title      = 'Test with %s' % self.__testDescription(),
                      xlabel     = 'Batch',
                      ylabel     = 'Work distribution',
                      maxY       = 100)

      self.__generateLatexFiguresSection(fileName           = name,
                                         title              = 'Worker Load distribution (%s)' % prover.id,
                                         figuresAndCaptions = [(name, 'Worker Load')])

    except:
      print "Error generating worker load plot"
      raise


  def generateUniqueResponsesPlot(self):
    try:
      name   = self.__mkFileName('unique_responses')
      series = []
      for prover in self.hcnfParams.provers:
        resultForProver = []
        series.append((self.__gnuplotEsc(prover.id), resultForProver))
        remainingProvers = filter(lambda x: x is not prover, self.hcnfParams.provers)
        for batch in range(1,self.numOfAvailBatches + 1):
          acum = 0
          answers = prover.proverStatsForBatch(batch).answer
          for test in range(self.hcnfParams.batchSize):
            if answers[test] != Statistics.ANSWER_TIMEOUT:
              otherAnswers = [p.proverStatsForBatch(batch).answer[test] for p in remainingProvers]
              if reduce(lambda x,y: x and (y == Statistics.ANSWER_TIMEOUT), otherAnswers, True):
                acum = acum + 1

          resultForProver.append(acum)

      generateBoxPlot(output     = name,
                      plotOutput = name,
                      series     = series,
                      title      = 'Test with %s' % self.__testDescription(),
                      xlabel     = 'Batch',
                      ylabel     = 'Number of unique responses',
                      maxY       = self.hcnfParams.batchSize)

      self.__generateLatexFiguresSection(fileName           = name,
                                         title              = 'Unique responses',
                                         figuresAndCaptions = [(name, 'Compared unique successful responses')])
      self.__newGnuplotScript('%s' % name)

    except:
      print "Error generating unique responses plot"
      raise


  def __extractSeriesFromStatistics(self, prover, statsAttr, shortName = '', baseName = None):
    if not baseName:
      baseName = prover.id

    if shortName != '':
      shortName = " (%s)" % shortName

    if prover.isPreprocessor():
      currentProverName = " [%s]" % prover.getPreprocessorId()
    else:
      currentProverName = ""

    baseName = prover.id
    again = True
    result = []

    while again:
      try:
        data = self.__extractSeriesValuesFromStatistics(prover, statsAttr)
        result.append( (self.__gnuplotEsc(baseName + currentProverName + shortName), data) )

      except AttributeError:
        # thrown by __extratSeriesValuesFromStatistics if statsAttr is not found
        # no data here, but there might be some in a wrapped prover
        pass

      if prover.isPreprocessor():
        prover = prover.getWrappedProver()
        currentProverName = " [%s]" % prover.id
      else:
        again = False

    return result


  def __extractSeriesValuesFromStatistics(self, prover, statsAttr):
    data = []
    for k in prover.sortedStatsKeys():
      stats = prover.proverStatsForBatch(k)
      attr  = getattr (stats, statsAttr)

      if callable(attr):
         data.append(attr())

      else:
         data.append(attr)

    return data


  def __sumSeries(self, series1, series2):
    return map(lambda (v1, v2): map(lambda (x, y): x + y, zip(v1, v2)), zip(series1, series2))


  def __divSeries(self, series1, series2):
    return map(lambda (v1, v2): map(lambda (x, y): x / y, zip(v1, v2)), zip(series1, series2))


  def __generateMaxPlotFromAttributes(self,
                                      output, plotOutput,
                                      statsAttrs,
                                      xlabel, ylabel,
                                      ylogscale = False,
                                      keyBoxed = False, keyPos = 'bmargin right horizontal'):
    series = []
    for prover in self.hcnfParams.provers:
      for (attr, shortName) in statsAttrs:
        seriesForProver = self.__extractSeriesFromStatistics(prover, attr, shortName)
        series.extend(seriesForProver)

    if len(series) == 0:
      raise NoAttributeFound

    generateMaxPlot   (output     = output,
                       plotOutput = plotOutput,
                       xdata      = self.__availableBatches(),
                       series     = series,
                       title      = 'Test with %s' % self.__testDescription(),
                       xlabel     = xlabel,
                       ylabel     = ylabel,
                       xrange     = self.__rangeForClauses(),
                       ylogscale  = ylogscale,
                       keyBoxed   = keyBoxed,
                       keyPos     = keyPos)



  def __generateMedianPlotFromAttributes(self,
                                         output, plotOutput,
                                         statsAttrs,
                                         xlabel, ylabel,
                                         ylogscale = False,
                                         withErrors = True,
                                         keyBoxed = False, keyPos = 'bmargin right horizontal'):

    series = []
    for prover in self.hcnfParams.provers:
      for (attr, shortName) in statsAttrs:
        seriesForProver = self.__extractSeriesFromStatistics(prover, attr, shortName)
        series.extend(seriesForProver)

    if len(series) == 0:
      raise NoAttributeFound

    generateMedianPlot(output     = output,
                       plotOutput = plotOutput,
                       xdata      = self.__availableBatches(),
                       series     = series,
                       title      = 'Test with %s' % self.__testDescription(),
                       xlabel     = xlabel,
                       ylabel     = ylabel,
                       xrange     = self.__rangeForClauses(),
                       ylogscale  = ylogscale,
                       withErrors = withErrors,
                       keyBoxed   = keyBoxed,
                       keyPos     = keyPos)


  def __rangeForClauses(self):
    return (self.hcnfParams.batches[0], self.hcnfParams.batches[-1])


  def __testDescription(self):
    p = self.hcnfParams
    return 'V = %i, N = %i, R = %i, D = %i, L = [%i{/Symbol \\274}%i]' % \
           (p.numOfProps, p.numOfNoms, p.numOfRels, p.diamDepth, p.fromNumClauses, p.toNumClauses)


  def __writeLatexFigure(self, o ,caption, graphicFile):
    o.write(r'\begin{center}' '\n')
    o.write(r'\includegraphics[width=0.8\textwidth,keepaspectratio=true]{%s.%s}' '\n' % (graphicFile, defaultPlotExt))
    o.write(r'\end{center}' '\n\n')


  def __mkFileName(self, name, subName = None):
    if subName:
      subName = "_" + subName.replace(' ', '+')
    else:
      subName = ""

    return '%s_%s%s' % (self.hcnfParams.testId,name,subName)


  def __latexEsc(self, s):
    return s.replace('_', r'\_').replace('^', r'\^').replace('#', r'\#')


  def __gnuplotEsc(self, s):
    return s.replace('_', r'\_').replace('^', r'\^').replace('@', r'\@')


  def __generateLatexFiguresSection(self, fileName, title, figuresAndCaptions):
    try:
      self.__addSection(fileName)
      f = open('%s.tex' % fileName, 'w')
      f.write('\paragraph{%s}\n\n' % title)

      for (figure, caption) in figuresAndCaptions:
        self.__writeLatexFigure(f, caption, figure)

      f.close()

    except:
      print "Error generating section file '%s.tex'!"
      raise


  def __getInconsistentAnswers(self):
    fact = self.hcnfParams.provers[-1]
    result = []
    for batch in range(1, self.numOfAvailBatches):
      for test in range(self.hcnfParams.batchSize):
        answersForTest = [prover.proverStatsForBatch(batch).answer[test] for prover in self.hcnfParams.provers]
        if not self.__isConsistentAnswer(answersForTest):
          result.append((batch, test))

    return result


  def __isConsistentAnswer(self, answers):
    noTimeOuts   = filter(lambda a: a != Statistics.ANSWER_TIMEOUT, answers)
    isConsistent = True
    while (isConsistent and len(noTimeOuts) > 0):
      isConsistent = noTimeOuts[0] == noTimeOuts.pop()

    return isConsistent


  def __answerCodeToHuman(self, code):
    if   code == Statistics.ANSWER_SAT:
      return 'Satisfiable'

    elif code == Statistics.ANSWER_UNSAT:
      return 'Unsatisfiable'

    elif code == Statistics.ANSWER_TIMEOUT:
      return 'Timeout'

    else: return 'Timeout'


  def __addSection(self, name):
    self.__latexSections.append(name)


  def __newGnuplotScript(self, name):
    self.__gnuplotScripts.append(name + ".gnuplot" )


  def __availableBatches(self):
    return self.hcnfParams.batches[0:self.numOfAvailBatches]


class NoAttributeFound(Exception):
  def __init__(self):
    Exception.__init__(self)
