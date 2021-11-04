# Usage: python3 ./profile.py [samples] [name]
#
# e.g., ./profile.py 1000 HelloWorld
#
import datetime
import sys
import statistics
import os

max_time = 0
values = []

for i in range(int(sys.argv[1])):
    a = datetime.datetime.now()
    os.system('./vmm '+sys.argv[2]) 
    b = datetime.datetime.now()
    c = b - a;
    max_time = max(max_time, c.total_seconds() * 1000)
    values.append(c.total_seconds() * 1000)    
f = open (sys.argv[2]+'.dat','w')
med = statistics.median(values);
for x in values:
    f.write(str((x - med) / med) + '\n');
f.close()
# plot using gnuplot
print("gnuplot -e \"set terminal png; set output '"+ sys.argv[2] + ".png';set xlabel 'max=" + str(max_time) + "ms, median="+ str(med) + "ms';set yzeroaxis;set boxwidth 0.05 absolute;set style fill solid 1.0 noborder;bin_width = 0.1;bin_number(x) = floor(x/bin_width);rounded(x) = bin_width * ( bin_number(x) + 0.5 ); show xlabel; set ylabel 'frecuency'; show ylabel; set title 'Average running time of "+ sys.argv[1]+" executions of " + sys.argv[2] +  "';set xrange [-3:3];set key off; plot '" + sys.argv[2] + ".dat' using (rounded(\$1)):(1) smooth frequency with boxes\"")
