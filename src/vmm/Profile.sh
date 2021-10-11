sum=0
cycles=100
for ((i = 0 ; i < $cycles ; i++)); do
  starttime=$(($(date +%s%N)/1000000))
  ./vmm HelloWorld
  endtime=$(($(date +%s%N)/1000000))
  sum=$((sum+endtime-starttime))
done
sum=$((sum/cycles))
echo "Average time in $cycles executions: $sum ms"
