set xrange [1:20];
set xlabel "Rank";
set ylabel "Speedup Factor";
set key off;

n = 5;
nchoose2 = (n*(n-1)/2);

fact(x) = gamma(x + 1);
choose(a, b) = fact(a) / (fact(b) * fact(a - b));

f(x) = (2 ** x) * nchoose2 / choose(x, x / 2)

plot f(x);

