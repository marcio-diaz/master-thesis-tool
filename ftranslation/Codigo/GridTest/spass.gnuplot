set term postscript eps enhanced color ;
set output 'spass.eps';

set autoscale fix;
set title 'SPASS time';
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

plot 0 title 'htab1', 'funcTrans_spass/data.dat' using 1:2 with lines title 'funcTrans', 'optimised_spass/data.dat' using 1:2 with lines title 'optimised', 'withoutSorts_spass/data.dat' using 1:2 with lines title 'withoutSorts', 'withoutSortsNoPNF_spass/data.dat' using 1:2 with lines title 'withoutSortsNoPNF';