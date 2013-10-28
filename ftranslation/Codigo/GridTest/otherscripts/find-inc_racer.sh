#!/bin/bash       

for gdepth in `seq 1 3`; do
  for pvars in `seq 3 10`; do
      for numclauses in `seq 3 50`;do   # at least 2 clauses
        echo Gdepth $gdepth Pvars $pvars Nomvars $nomvars Numclauses $numclauses
        rm -Rf X
        mkdir X
        cd X
        ../hgen --global-depth=$gdepth    \
                --prop-vars=$pvars        \
                --nom-vars=0              \
                --num-clauses=$numclauses \
                --num-inst=20             \
                --clause-size=[0,1] > /dev/null
        cd ..
        for i in $( ls X); do
          ./convHTab X/$i  -t 15 > resphtab 
          ./convRacer X/$i 20  > respracer
          if grep -q "is unsatisfiable" resphtab ; then
            if grep -q "t$" respracer ; then
              cp X/$i Inconsistent
              echo "HTab UNSAT, Racer SAT"
            fi
          elif grep -q "is satisfiable" resphtab ; then
            if egrep -q "nil$" respracer ; then
              cp X/$i Inconsistent
              echo "HTab SAT, Racer UNSAT"
            fi
          fi
       done
     done
  done
done
exit 

