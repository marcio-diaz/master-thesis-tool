# -*- coding: iso-8859-1 -*-
""" pellet module
"""

import re
from prover import Prover, Statistics, NoParse
from miscFunctions import *

class PELLET(Prover):
    v1_3   = "v1.3"
  #  v2_1   = "2.1"
  #  v1_0_3 = "1.0.3"

    def __init__(self, id, binDir, binName = 'pellet.sh', version = v1_3):
        Prover.__init__(self,id)
        self.__binLocation = '%s/%s' % (binDir, binName)
        #self.__sortsParam  = sorts       #default is 1, but layered translation seems to work better with 0
        self.__version     = version

    def requiredBinaries(self):
        return [self.__binLocation]

    def requiredFiles(self):
        return []

    def run(self, batchDir, testFile, timeout):
        # OMGWTFBBQ!!!!!
 

        pelletCmdLine  = ['/global/vienville/ale/modaltesting/bin/pellet.sh -if file:///users/led/lorenzoa/proof.owl -inputFormat RDF/XML -unsat -s off -consistency off -timeout %i' % timeout] 

        self.runWrapper(pelletCmdLine, testFile)

        pelletResponse = '%s' % self.proverResponse(testFile)
        f = open(pelletResponse,'r')  
        flines = " ".join(f.readlines())
        #UNSAT = re.match('Unsatisfiable Concepts',flines)
        f.close()
        if (flines.find('Exception:') > -1) or (flines.find('Error:') > -1): 
                f= open(pelletResponse,'a+')
                f.write('\n TIMEOUT: EXCEPTION or ERROR')
                print "Le pongo TIMEOUT POR EXCEPTION"
                f.close()
        elif (flines.find('Unsatisfiable Concepts') < 0) and (flines.find('TIMEOUT:') < 0):
                f= open(pelletResponse,'a+')
                f.write('\n Satisfiable Concepts')
                print "Le pongo sat"
                f.close()
        else: print "Es unsat o timeout"
        #print "UNSAT=" + str(UNSAT) + " en lineas:  " + flines
        
        #else: print "nada"
        ##print file(pelletResponse).readlines()
        #f.close()
            
    def newStatistics(self):
       return PelletStatistics(self.__version)
                
    def getConfiguration(self):
        return  [("Prover", "pellet"),
                 ("Version", self.__version),
                 ("Configuration", "-Auto=1 -Sorts=" )]


class PelletStatistics(Statistics):
    def __init__(self, pelletVersion):
        Statistics.__init__(self)
        self.__pelletVersion = pelletVersion
        self.derivedClauses = []
        self.backtrackedClauses = []
        self.keptClauses = []

    def addNewColumn(self):
        Statistics.addNewColumn(self)
        self.derivedClauses.append(0)
        self.backtrackedClauses.append(0)
        self.keptClauses.append(0)

    def parseResponse(self, pelletResponseFile):
        try:
            #print ('%s') % pelletResponseFile
            f = open(pelletResponseFile)  
            #print file(pelletResponseFile).readlines()
            #assert(f.readline() == '\n')
            #self.acceptHeader(file(pelletResponseFile).readline())
            #assert(f.readline() == '\n')
            self.parseResult(" ".join(file(pelletResponseFile).readlines()))
            #self.acceptProblemName(f.readline())
            # self.parseClauses(f.readline()) Ver, esto no se hace no?
            f.close()
        except:
            print "Error parsing '%s'" % pelletResponseFile
            raise

    def acceptHeader(self, line):
        """ Accepts the 'SPASS V <version>' line """
        assert( self.accept(r'^Input file: file:///users/led/lorenzoa/proof.owl', '__headerParser', line) )

    def parseResult(self, line):
        """ Finds out if the formula was proven satisfiable, unsatisfiable or the timeouted """
        #print line 
        #lineaux = " ".join(line)
        #line = lineaux
        #print "lineaux " + lineaux + " fin lineaux"
        parser = self.retrieveParser(r'(Unsatisfiable Concepts)|(Satisfiable Concepts)|(TIMEOUT:)',
                                     '__satUnsatTimeoutParser')
        matched = parser.search(line)
        assert( matched )
        if   matched.group(1):
            self.replaceLastVal(self.answer, Statistics.ANSWER_UNSAT)            
        elif matched.group(2):
            self.replaceLastVal(self.answer, Statistics.ANSWER_SAT)
        elif matched.group(3):
            self.replaceLastVal(self.answer, Statistics.ANSWER_TIMEOUT)
        else: assert(False)
        

    def acceptProblemName(self, line):
        """ Accepts the line that shows the problema name """
        assert( self.accept(r'^Problem:', '__problemNameParser', line) )

    def parseClauses(self, line):
        """ Parses the number of clauses generated """
        #parser = self.retrieveParser(r'^SPASS derived (\d+) clauses, backtracked (\d+) clauses and kept (\d+) clauses.$',
                                     #'__clausesParser')
        #matched = parser.search(line)
        #assert( matched )
        #self.replaceLastVal(self.derivedClauses, int(matched.group(1)))
        #self.replaceLastVal(self.backtrackedClauses , int(matched.group(2)))
        #self.replaceLastVal(self.keptClauses , int(matched.group(3)))

    def parseTime(self, timeResponseFile):
        try:
            self._parseTimeSkipping(0, timeResponseFile)
        except NoParse:
            # If SPASS segfaulted, then the first line will say
            # "Command terminated by signal 11"
            self._parseTimeSkipping(1, timeResponseFile)
