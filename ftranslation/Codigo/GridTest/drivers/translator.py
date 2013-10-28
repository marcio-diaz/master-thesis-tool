# -*- coding: iso-8859-1 -*-
""" Translator module.
    Suitable for any translator that take two arguments, input
    and output file, respectively.
"""


import os

from prover        import Prover, Statistics
from miscFunctions import *


class Translator(Prover):
    def __init__(self, translatorId, binDir, binName, prover):
        Prover.__init__(self, '%s+%s' % (prover.id, translatorId))
        self.translatorId = translatorId
        self.prover =  prover
        self.binDir =  binDir
        self.binName = binName


    def requiredBinaries(self):
        return ['%s/%s' % (self.binDir, self.binName)] + self.prover.requiredBinaries()


    def requiredFiles(self):
        return self.prover.requiredFiles()


    def isPreprocessor(self):
        return True

    def getWrappedProver(self):
        return self.prover


    def addNewStatsToBatch(self, batch):
        proverStats = self.prover.addNewStatsToBatch(batch)
        myStats = Prover.addNewStatsToBatch(self,batch)
        myStats.aliasWith(proverStats)
        return myStats

    def newStatistics(self):
        return TranslatorStats()

    def parseOutputFilesFor(self, batch, testFile):
        self.prover.parseOutputFilesFor(batch, self.translatorResponse(testFile))
        stats = self.proverStatsForBatch(batch)
        stats.parseTime(self.timeResponse(testFile))


    def run(self, batchDir, testFile, timeout):
        translatedFile = self.translatorResponse(testFile)
        self.runTranslation(batchDir, testFile, translatedFile)
        # Warning, total execution time (translator + prover) might be
        # exceeding the maximum timeout!
        self.prover.run('.', translatedFile, timeout)


    def runTranslation(self, batchDir, testFile, translatedFile):
        translatorCmdLine = ['%s/%s' % (self.binDir, self.binName),
                             '%s/%s' % (batchDir, testFile),
                             '%s'    % translatedFile]

        self.runWrapper(translatorCmdLine, testFile)


    def getConfiguration(self):
        return [("Translator", self.translatorId),
                ("Binary location", "%s/%s" % (self.binDir, self.binName))] + self.prover.getConfiguration()

    def getPreprocessorId(self):
        return self.translatorId

    def timeResponse(self, testFile):
        return testFile + '.translation.time'


    def proverResponse(self, testFile):
        return self.prover.proverResponse(self.translatorResponse(testFile))

    def translatorResponse(self, testFile):
        return testFile + '.translated'

class TranslatorStats(Statistics):
    def __init__(self):
        Statistics.__init__(self)
        self.proverStats = None

    def aliasWith(self, proverStats):
        self.proverStats = proverStats
        self.answer      = proverStats.answer

    def getStatValues(self):
        trans_stats_dict = Statistics.getStatValues(self)
        # this one is aliased, so we don't export it twice
        # however, we need to provide a value for it,
        # otherwise setStatValues would fail
        trans_stats_dict["answer"] = None
        # technically, it would be better to save the class of the prover
        trans_stats_dict["proverStatsClass"] = self.proverStats.__class__
        trans_stats_dict["proverStats"] = self.proverStats.getStatValues()
        return trans_stats_dict

    def setStatValues(self, stats):
        Statistics.setStatValues(self, stats)

        # remember: stats["proverStatsClass"] is a class object
        proverStats = stats["proverStatsClass"]()
        proverStats.setStatValues(stats["proverStats"])

        self.aliasWith(proverStats)

    def consolidate(self, stats):
        listFields = self.getStatValues().keys()
        listFields.remove("answer")  #this one is aliased; it would be extended twice
        listFields.remove("proverStatsClass")
        listFields.remove("proverStats")

        self.consolidateTheseLists(stats, listFields)

        self.proverStats.consolidate(stats.proverStats)

    def addNewColumn(self):
        self.answer = []
        Statistics.addNewColumn(self)
        self.proverStats.addNewColumn()
        self.answer = self.proverStats.answer
