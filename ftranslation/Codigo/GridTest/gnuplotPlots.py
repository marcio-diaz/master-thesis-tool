# -*- coding: utf-8 -*-
""" GNUPlot module
"""


from math import ceil, log
from prover import Statistics
from string import join





defaultPlotExt = 'eps'



def generatePlot(output,
                 xdata, series,
                 title, xlabel, ylabel,
                 plotOutput = None,
                 xrange = None, maxY = None,
                 ylogscale = False, seriesError = None,
                 keyBoxed = False, keyPos = 'bmargin right horizontal'):

  # .dat
  # arrange in columns separated by one space character
  handle_data = open("%s.dat" % output, "w")

  if seriesError:
    titleline = join(["#X"] + [seriesName + "(y, ylow, yhigh)" for (seriesName,_) in series], "; ") +"\n"
  else:
    titleline = join(["#X"] + [seriesName for (seriesName,_) in series] + ["\n"])

  handle_data.write(titleline)

  if seriesError:
    seriesWithError = zip(series, seriesError)
    for idx in range(len(xdata)):
      yvals = [map(str,(ydata[idx],yerr[idx][0],yerr[idx][1])) for ((_, ydata), yerr) in seriesWithError]
      handle_data.write(join([str(xdata[idx])] + map(join, yvals)) + "\n")
  else:
    for idx in range(len(xdata)):
      vals = [xdata[idx]] + [ydata[idx] for (_,ydata) in series]
      handle_data.write(join(map(str, vals)) + "\n")


  handle_data.close()

  # .gnuplot
  handle_gnuplot = open("%s.gnuplot" % output, "w")
  _writePlotHeader(output     = handle_gnuplot,
                   title      = title,
                   xlabel     = xlabel,
                   ylabel     = ylabel,
                   plotOutput = plotOutput,
                   xrange     = xrange,
                   maxY       = maxY,
                   ylogscale  = ylogscale,
                   keyBoxed   = keyBoxed,
                   keyPos     = keyPos)

  handle_gnuplot.write('plot ')
  for (i, (seriesName, _)) in enumerate(series):
    if i != 0: handle_gnuplot.write(', ')

    if seriesError:
      handle_gnuplot.write("'%s.dat' using 1:%i:%i:%i title '%s' with yerrorlines" % (output, (3 * i) + 2, (3 * i) + 3, (3 * i) + 4, seriesName))
    else:
      handle_gnuplot.write("'%s.dat' using 1:%i       title '%s' with linespoints" % (output,       i + 2,                           seriesName))

  handle_gnuplot.write(';\n')
  handle_gnuplot.close()


def generateMaxPlot(output,
                    xdata, series,
                    title, xlabel, ylabel,
                    plotOutput = None,
                    xrange = None, ylogscale = False,
                    keyBoxed = False, keyPos = 'bmargin right horizontal'):

  maxY             = 0
  seriesWithMax = []

  for (seriesName, seriesData) in series:
    # add 0.1 to all data to avoid cases where median is 0
    seriesData = map(lambda x: map(lambda y: y + 0.1, x), seriesData)
    seriesWithMax.append((seriesName, map(lambda vec: '%s' % max(vec), seriesData)))

    for values in seriesData:
      # append 0 to the list just to make it not null
      filteredValues = filter(lambda x: x < Statistics.INFINITY, values)
      filteredValues.append(0)
      maxY = max(maxY, max(filteredValues))

  generatePlot(output      = output,
               xdata       = xdata,
               series      = seriesWithMax,
               title       = title,
               xlabel      = xlabel,
               ylabel      = ylabel,
               plotOutput  = plotOutput,
               xrange      = xrange,
               maxY        = maxY,
               ylogscale   = ylogscale,
               seriesError = None,
               keyBoxed    = keyBoxed,
               keyPos      = keyPos)


def generateMedianPlot(output,
                       xdata, series,
                       title, xlabel, ylabel,
                       plotOutput = None,
                       xrange = None, ylogscale = False, withErrors = False,
                       keyBoxed = False, keyPos = 'bmargin right horizontal'):

  maxY             = 0
  seriesWithMedian = []
  if withErrors:
    seriesError = []
  else:
    seriesError = None

  for (seriesName, seriesData) in series:
    # add 0.1 to all data to avoid cases where median is 0
    seriesData = map(lambda x: map(lambda y: y + 0.1, x), seriesData)
    seriesWithMedian.append((seriesName, map(lambda vec: '%s' % median(vec), seriesData)))
    if withErrors:
      seriesError.append(map(lambda vec: (medianBelowHalf(vec), medianAboveHalf(vec)), seriesData))

    for values in seriesData:
      # append 0 to the list just to make it not null
      filteredValues = filter(lambda x: x < Statistics.INFINITY, values)
      filteredValues.append(0)
      maxY = max(maxY, max(filteredValues))

  generatePlot(output      = output,
               xdata       = xdata,
               series      = seriesWithMedian,
               title       = title,
               xlabel      = xlabel,
               ylabel      = ylabel,
               plotOutput  = plotOutput,
               xrange      = xrange,
               maxY        = maxY,
               ylogscale   = ylogscale,
               seriesError = seriesError,
               keyBoxed    = keyBoxed,
               keyPos      = keyPos)


def bounded(c, n):
  return c[max(0, min(len(c) - 1, n))]


def median(li):
  return bounded(sorted(li), len(li) / 2)


def medianBelowHalf(li): #NEW
  return bounded(sorted(li), len(li) / 4)


def medianAboveHalf(li): #NEW
  return bounded(sorted(li), int(ceil(len(li) / 1.25)))


def generateBoxPlot(output,
                    series,
                    title, xlabel, ylabel,
                    maxY = None,
                    plotOutput = None,
                    keyBoxed = False, keyPos = 'bmargin right horizontal',
                    pointSep = 3):

  numberOfSeries       = len(series)
  pointWidth           = pointSep + numberOfSeries
  (_, firstSeriesData) = series[0]
  numberOfPoints       = len(firstSeriesData)

  # .dat
  handle_data = open("%s.dat" % output, "w")

  titleline = []
  for idx in range(len(series)):
    (seriesName, _) = series[idx]
    titleline +=  ["X%i" %idx] + [seriesName]

  handle_data.write(" ".join(titleline + ["\n"]))

  xdata = {}
  for i in range(numberOfSeries):
    xdata[i] = range(pointSep + i, numberOfPoints * pointWidth, pointWidth)

  for idx in range(len(xdata[0])):
    line = []
    for idx2 in range(numberOfSeries):
      line += [xdata[idx2][idx]]
      (_, seriesData) = series[idx2]
      line += [seriesData[idx]]

    line += ["\n"]
    handle_data.write(" ".join(map(str,line)))

  handle_data.close()

  # .gnuplot
  handle_gnuplot = open("%s.gnuplot" % output, "w")

  xtics = ''
  for i in range(numberOfPoints):
    if i != 0: xtics = xtics + ', '
    xtics = xtics + '"%s" %i' % (i + 1, pointWidth * i + ceil(pointWidth / 2.0))

  _writePlotHeader(output     = handle_gnuplot,
                   plotOutput = plotOutput,
                   title      = title,
                   xlabel     = xlabel,
                   xrange     = (0, numberOfPoints * pointWidth + 1),
                   xtics      = '(%s)' % xtics,
                   ylabel     = ylabel,
                   maxY       = maxY,
                   ylogscale  = False,
                   keyBoxed   = keyBoxed,
                   keyPos     = keyPos)

  handle_gnuplot.write('set style fill solid 0.5 border -1;\n')
  handle_gnuplot.write('set boxwidth 1\n')

  handle_gnuplot.write('plot ')
  for (i, (seriesName, _)) in enumerate(series):
    if i != 0: handle_gnuplot.write(', ')
    handle_gnuplot.write("'%s.dat' using %i:%i title '%s' with boxes" % (output, 2 * i + 1, 2 * i + 2, seriesName))

  handle_gnuplot.write(';\n')
  handle_gnuplot.close()


def _writePlotHeader(output,
                     title, xlabel, ylabel,
                     plotOutput = None,
                     xrange = None, maxY = None,
                     ylogscale = False,
                     xtics = 2,
                     keyBoxed = False, keyPos = 'bmargin right horizontal'):

  if plotOutput:
    output.write("set term postscript eps enhanced color ;\n")
    output.write("set output '%s.%s';\n" % (plotOutput, defaultPlotExt))

  output.write("\n")
  output.write("set autoscale fix;\n")
  output.write("set title '%s';\n" % title)
  output.write("set xlabel '%s';\n" % xlabel)
  output.write("set ylabel '%s';\n" % ylabel)
  if xrange != None:
    output.write("set xrange [%i:%i];\n" % xrange)

  if maxY != None and maxY > 0:
    if ylogscale:
      output.write("set yrange [*:%i];\n" % int(10 ** (ceil(log(maxY, 10)))))
      output.write(r'set format y "10^{%L}";' + "\n")
    else:
      output.write("set yrange [*:%f];\n" % maxY)

  output.write("set ytics nomirror;\n")
  output.write("set xtics nomirror %s;\n" % xtics)
  output.write("set nologscale;\n")
  if ylogscale:
    output.write("set logscale y;\n")

  if keyBoxed:
    output.write("set key %s box;\n" % keyPos)
  else:
    output.write("set key %s;\n" % keyPos)

  output.write('set tics out\n\n')

  output.write("\n")
