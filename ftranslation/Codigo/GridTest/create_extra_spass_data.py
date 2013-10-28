from os import listdir
from os.path import isdir, isfile, join

def my_str_key(s):
    s = s.split("batch-")
    k = int(s[1])
    return k

def read_dirs(path, timeout):
    dirs = [f for f in listdir(path) 
                     if isdir(join(path, f))]
    dirs.remove("htab1")
    dirs.sort()
#    print dirs
    for d in dirs:
        dp = join(path, d)
#        print dp
        
        batches = [join(dp, f) for f in listdir(dp)]

        batches = sorted(batches, lambda x, y: cmp(int(my_str_key(x)), int(my_str_key(y))))
#        print batches
        data_file = open(join(dp, 'data.dat'), 'a')
#        print data_file
        bc = 0
        for b in batches:
            if isdir(b):
                bc += 1
                ps_tot = 0.0
                fs_tot = 0.0
                diff_tot = 0.0
                cn_tot = 0
                c = 0
                for f in listdir(b):
                    if f.find("response") != -1:
                        ff = join(b, f)
                        ps, fs, diff, cn = read_times_from_file(ff)
                        if float(ps) >= float(timeout):
                            continue
                        if fs == 0: 
                            fs = ps
                            diff = 0.0
                        ps_tot += ps
                        fs_tot += fs
                        diff_tot += diff
                        cn_tot += int(cn)
                        c += 1
                if c > 0:
                    line =  "%s %s %s %s %s\n" % (bc, ps_tot/c, fs_tot/c, diff_tot/c, cn_tot/c)
                    #                print line
                    data_file.write(line)
        data_file.close()

def read_times_from_file(name):
    f = open(name)
    lines = f.readlines()
    f.close()
    problem_line = None
    flotter_line = None
    clause_line = None
    for l in lines:
        if l.find("problem") != -1:
            problem_line = l
        elif l.find("FLOTTER") != -1:
            flotter_line = l
        elif l.find("derived") != -1:
            clause_line = l
    problem_line_list = problem_line.split()
    flotter_line_list = flotter_line.split()
    clause_line_list = clause_line.split()
    problem_time = problem_line_list[2]
    flotter_time = flotter_line_list[0]
    clause_num = clause_line_list[12]
#    print "problem time %s" % problem_time
#    print "flotter time %s" % flotter_time
    
    problem_hours, problem_mins, problem_secs = problem_time.split(':')
    flotter_hours, flotter_mins, flotter_secs = flotter_time.split(':')
    
    problem_secs = float(problem_secs) + int(problem_mins) * 60 + int(problem_hours) * 60 * 60
    flotter_secs = float(flotter_secs) + int(flotter_mins) * 60 + int(flotter_hours) * 60 * 60
    
    diff = problem_secs - flotter_secs
    return (problem_secs, flotter_secs, diff, clause_num)

if __name__ == "__main__":
    import sys
    read_dirs(sys.argv[1], sys.argv[2])
