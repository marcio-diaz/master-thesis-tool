set term postscript eps enhanced color ;
set output 'flotter.eps';

set autoscale fix;
set title 'FLOTTER time';
set xlabel '';
set ylabel 'Median execution time (s)';
set xrange [1:7];
set yrange [*:120];
#set format y "10^{%L}";
set ytics nomirror;
set xtics nomirror 2;
set nologscale;
#set logscale y;
set key bmargin right horizontal;
set tics out

plot 0 title 'htab1', 'funcTrans_spass/data.dat' using 1:3 with lines title 'funcTrans', 'optimised_spass/data.dat' using 1:3 with lines title 'optimised', 'withoutSorts_spass/data.dat' using 1:3 with lines title 'withoutSorts', 'withoutSortsNoPNF_spass/data.dat' using 1:3 with lines title 'withoutSortsNoPNF';