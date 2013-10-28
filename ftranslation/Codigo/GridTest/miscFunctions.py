# -*- coding: utf-8 -*-
""" Miscelaneous functions
"""


import sys
import os
import pickle
import math
import datetime

from stat import *





def setinc(dict, key, default, inc):
  dict[key] = dict.setdefault(key, default) + inc


def divUp(i, j):
  return int(math.ceil(float(i) / float(j)))



def fullabsexpand(path):
  return os.path.expanduser(os.path.expandvars(os.path.abspath(path)))


def fullexpand(path):
  return os.path.expanduser(os.path.expandvars(path))



def fullsplit(path):
  (dirname, temp) = os.path.split(fullexpand(path))
  (basename, ext) = os.path.splitext(temp)
  return (dirname, basename, ext)


def filesplit(path):
  (dirname, file) = os.path.split(path)
  return file



def timestring(secs):
  return '%d:%02d:%02d' % ((secs // 60) // 60, (secs // 60) % 60, secs % 60)


def nowString():
  return datetime.datetime.today().isoformat(' ')



def destroyScratch(test):
  if test.dirStructure.scratchDir != None:
    os.system("rm -rf %s" % fullexpand(test.dirStructure.scratchDir))



def save(obj, path):
  f = open(fullexpand(path), 'w')
  pickle.dump(obj, f)
  f.close()


def load(path):
  f   = open(fullexpand(path), 'r')
  obj = pickle.load(f)
  f.close()
  return obj



def assertOne(i, msg_i):
  if i != 1:
    sys.exit(('Error: %s (' + str(i) + ') must be equal to 1!') % msg_i)


def assertEq(i, j, msg_i, msg_j):
  if i != j:
    sys.exit(('Error: %s (' + str(i) + ') must be equal to %s (' + str(j) + ')!') % (msg_i, msg_j))


def assertGt(i, j, msg_i, msg_j):
  if i <= j:
    sys.exit(('Error: %s (' + str(i) + ') must be greater than %s (' + str(j) + ')!') % (msg_i, msg_j))


def assertBetween(a, i, j, msg_a, msg_i = None, msg_j = None):
  if not (i <= a and a <= j):
    if not msg_i: msg_i = str(i)
    if not msg_j: msg_j = str(j)
    sys.exit(('Error: %s must be between %s and %s!') % (msg_a, msg_i, msg_j))


def assertPositive(i, msg):
  if i < 0:
    sys.exit(('Error: %s (' + str(i) + ') must be positive!') % msg)


def assertGtZero(i, msg):
  if i < 1:
    sys.exit(('Error: %s (' + str(i) + ') must be greater than zero!') % msg)


def assertIsZeroIffIsZero(i, j, iname, jname):
  if (i == 0 and j != 0) or (i != 0 and j == 0):
    sys.exit(('Error: %s (' + str(i) + ') and %s (' + str(j) + ') must be both zero or both nonzero!') % (iname, jname))


def assertIsZeroThenIsZero(i, j, iname, jname):
  if (i == 0 and j != 0):
    sys.exit(("Error: %s (" + str(i) + ") can't be zero when %s (" + str(j) + ") is zero!") % (jname, iname))


def assertExecExists(execPath):
   xexecPath = fullexpand(execPath)
   if not os.access(xexecPath, os.X_OK):
     sys.exit("Error: '%s' not found or is not runnable!" % xexecPath)


def assertFileExists(filePath):
   xfilePath = fullexpand(filePath)
   if not os.access(xfilePath, os.F_OK):
     sys.exit("Error: '%s' not found!" % xfilePath)


def assertIsADirectory(filePath):
   xfilePath = fullexpand(filePath)
   if not isADirectory(xfilePath):
     sys.exit("Error: '%s' is not a directory!" % xfilePath)


def isADirectory(filePath):
  xfilePath = fullexpand(filePath)
  return os.access(xfilePath, os.F_OK) and S_ISDIR(os.stat(xfilePath)[ST_MODE])



def mkdirsOrDie(path):
  xpath = fullexpand(path)
  try:
    if not os.access(xpath, os.F_OK): os.makedirs(xpath)

  except:
    print "Error: Couldn't create directory '%s'!" % xpath
    raise


def chdirOrDie(path):
  xpath = fullexpand(path)
  try:
    os.chdir(xpath)

  except:
    print "Error: Couldn't change directory to '%s'!" % xpath
    raise


def removeOrDie(path):
  xpath = fullexpand(path)
  try:
    if os.access(xpath, os.F_OK): os.remove(xpath)

  except:
    print "Error: Couldn't delete file '%s'!" % xpath
    raise



def sortedListDir(path):
  xpath = fullexpand(path)
  l = os.listdir(xpath)
  l.sort()
  return l



def configureTime():
  def silentRun(p):
    return systemRedir(p[0], p[1:], redirStdOut = None, redirStdErr = None)

  # we will do some probing, based on the assumption that /bin/true
  # is installed
  if silentRun(['true']) != 0:
    sys.exit('true not found')

  # some systems (seen on grid5000.fr) require a --quiet flag to avoid
  # dumping a lot of additional information (that we don't want). since
  # other implementations of time don't know about the --quiet flag, we
  # have to test it explicitly.... (sigh)
  if silentRun(['time', '--quiet', '-p', 'true']) == 0:
    return ['time', '--quiet', '-p']

  if silentRun(['time', '-p', 'true']) == 0:
    return ['time', '-p']

  # if bash's internal time is used, we must include -p as part of the
  # command name; otherwise it gets interpreted as the program to run
  if silentRun(['time -p', 'true']) == 0:
    return ['time -p']

  sys.exit('No suitable time command found!')



def systemRedir(prg, args, redirStdOut = False, redirStdErr = False):
  xprg = fullexpand(prg)
  cmd = '(%s)' % reduce(lambda s, t: "%s '%s'" % (s, t), args, xprg)
  if redirStdOut != False:
    if redirStdOut == None:
      redirStdOut = '/dev/null'

    cmd = "%s > '%s'" % (cmd, redirStdOut)

  if redirStdErr != False:
    if redirStdErr == None:
      redirStdErr = '/dev/null'

    cmd = "%s 2> '%s'" % (cmd, redirStdErr)
  return os.WEXITSTATUS(os.system(cmd))



def systemOrDie(prg, args, allowedErrorCodes = [], **redirections):
  # handle output redirections
  stdout = redirections.get('stdout', False)
  stderr = redirections.get('stderr', False)

  result = systemRedir(prg, args, redirStdOut = stdout, redirStdErr = stderr)
  if result != 0 and not result in allowedErrorCodes:
    pass
#    raise AssertionError("Invalid error code (%i,%s,%s)" % (result, prg, args))
  return result

def which(prgName):
  path = os.environ['PATH']
  for p in path.split(os.pathsep):
    fullName = os.path.join(p, prgName)
    if os.access(fullName, os.X_OK):
      return fullName

  return None
