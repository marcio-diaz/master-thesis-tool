from subprocess import call
from os.path import join
import os

def create_graphs(scratch_dir):
    os.chdir(scratch_dir)
    call(["gnuplot", "../../flotter.gnuplot"])
    call(["gnuplot", "../../spass.gnuplot"])
    call(["gnuplot", "../../diff.gnuplot"])
    call(["gnuplot", "../../clausules.gnuplot"])

if __name__ == "__main__":
    import sys
    create_graphs(sys.argv[1])
