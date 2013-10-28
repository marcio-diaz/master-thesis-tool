from os import listdir
from os.path import isfile, join
from random import sample
from shutil import copyfile

def copy_dir_random(src_dir_path, dest_dir_path, num_files):
    src_dir_files = [f for f in listdir(src_dir_path) if isfile(join(src_dir_path, f))]
    dest_dir_file = src_dir_files[0]
    c = 0
    for f in range(0, num_files):
        copyfile(join(src_dir_path, dest_dir_file), join(dest_dir_path, dest_dir_file+str(c)))
        c += 1
