#!/bin/bash       

for gdepth in `seq 1 3`; do
  for pvars in `seq 1 4`; do
    for nomvars in `seq 1 4`; do
      for numclauses in `seq 1 10`;do
       for puniv in `seq 0 1`; do
       for pdiff in `seq 0 1`; do
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
                --num-inst=40            \
                --clause-size=[0,1] > /dev/null
        cd ..
        for i in $( ls X); do
          ./htab -t 10 -f X/$i > resphtab -m X/$i.htab.m
          ./hylores -t 10 -f X/$i -m X/$i.hylores.m > resphylores
          if grep -q "is unsatisfiable" resphtab ; then
            if grep -q "is satisfiable" resphylores ; then
              cp X/$i Inconsistent
              echo "HTab UNSAT, HyLoRes SAT"
            fi
          elif grep -q "is satisfiable" resphtab ; then
            if grep -q "is unsatisfiable" resphylores ; then
              cp X/$i Inconsistent
              echo "HTab SAT, HyLoRes UNSAT"
            elif (./mcheck X/$i.htab.m X/$i | grep -q "False"); then
              echo "HTab bad model"
              cp X/$i X/$i.htab.m BadModel
            fi
          fi
          if grep -q "is satisfiable" resphylores ; then
             if (./mcheck X/$i.hylores.m X/$i | grep -q "False"); then
              echo "HyLoRes bad model"
              cp X/$i X/$i.hylores.m BadModel
             fi
          fi
        done
        done
       done
      done
    done
  done
done
exit 

