set xrange [1:20];
set xlabel "Rank";
set ylabel "Speedup Factor";
set key off;

fact(x) = gamma(x + 1);
choose(a, b) = fact(a) / (fact(b) * fact(a - b));

plot 2 ** x / choose(x, x/2);

