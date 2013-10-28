#!/usr/bin/env python
# -*- coding: utf-8 -*-


from miscFunctions import *

import sys
import os





def usage():
  print ("Usage: %s <nodes> <cores> <time> <command>\n") % (sys.argv[0])
  sys.exit(1)





if __name__ == "__main__":
  argc = len(sys.argv)
  if argc != 5:
    usage()

  nodes   = int(sys.argv[1])
  cores   = int(sys.argv[2])
  time    =     sys.argv[3]
  command =     sys.argv[4]

  systemOrDie('oarsub -l /nodes=%d/core=%d,walltime=%s "%s" > oarsubtemp' % (nodes, cores, time, command), [])  # submit job

  # parse oarsubtemp
  f = open('oarsubtemp', 'r')                 # open oarsubtemp for reading
  f.readline()                                # throw away the first line
  # f.readline()                                # and the second
  line = f.readline()                         # load the third
  jobid = int (line[line.rfind('=') + 1:])    # the job id is what's left to the right of the last '='
  f.close()                                   # be tidy about files

  systemOrDie('rm -f oarsubtemp',         []) # remove oarsubtemp
  systemOrDie('oarstat -f -j %d' % jobid, []) # ask for the job
