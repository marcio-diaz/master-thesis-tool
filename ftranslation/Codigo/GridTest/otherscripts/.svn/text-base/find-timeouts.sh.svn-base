#!/bin/bash       

#!/bin/bash

for gdepth in `seq 1 4`; do
  for pvars in `seq 1 5`; do
    for nomvars in `seq 1 5`; do
      for numclauses in `seq 3 15`;do
       for puniv in `seq 1 1`; do
       for pdiff in `seq 1 1`; do
        echo Gdepth $gdepth Pvars $pvars Nomvars $nomvars Numclauses $numclauses Puniv $puniv Pdiff $pdiff
        rm -Rf X
        mkdir X
        cd X
        ../hgen --diff-univ-mod-depth=$pdiff \
                --proba-diff=$pdiff \
                --univ-mod-depth=$puniv   \
                --proba-univ=$puniv       \
                --global-depth=$gdepth    \
                --prop-vars=$pvars        \
                --nom-vars=$nomvars       \
                --num-clauses=$numclauses \
                --num-inst=20            \
                --clause-size=[0,1]  > /dev/null
        cd ..
        for i in $( ls X); do
          ./htab -c 0 -t 80 -f X/$i > resphtab -m X/$i.htab.m
          if grep -q "is satisfiable" resphtab ; then
            if (./mcheck X/$i.htab.m X/$i | grep -q "False"); then
              echo "HTab bad model"
              cp X/$i X/$i.htab.m BadModel
            fi
          elif grep -q "TIMEOUT" resphtab ; then
             echo "HTab Timeout"
             cp X/$i Timeouted
          fi
        done
        done
       done
      done
    done
  done
done
exit 

