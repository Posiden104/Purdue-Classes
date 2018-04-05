#!/bin/bash
(cd ../;make clean;make)
sudo chmod +x fibonacci
sudo chmod +x multiply
sudo chmod +x quicksort
rm -f total.txt
rm -f *.out
total=0
totalmax=0

# Test driver
function runtest {
  prog=$1
  input=$2
  des=$3
  grade=$4
  totalmax=`expr $totalmax + $grade`
  echo ----- $prog: "$des" -----
  cat $input | ../$prog > $des.user.out
  cat $input | ./$prog > $des.test.out
  diff $des.user.out $des.test.out
  if [ $? -eq 0 ]; then
      echo $des passed...;
      printf "%-36s: passed : %-3d of %-3d\n" "$des" $grade $grade >>total.txt
      total=`expr $total + $grade`;
  else
      echo $des failed...;
      printf "%-36s: failed : %-3d of %-3d\n" "$des" 0 $grade >> total.txt
  fi
}
runtest fibonacci fibonacci1 "fibonacci1" 5;
runtest fibonacci fibonacci2 "fibonacci2" 5;
runtest fibonacci fibonacci3 "fibonacci3" 5; 
runtest fibonacci fibonacci4 "fibonacci4" 5; 
runtest multiply multiply1 "multiply1" 5;
runtest multiply multiply2 "multiply2" 5;
runtest multiply multiply3 "multiply3" 5;
runtest multiply multiply4 "multiply4" 5;
runtest multiply multiply5 "multiply5" 5;
runtest multiply multiply6 "multiply6" 5;
runtest quicksort quicksort_negative1 "quicksort_negative1" 5;
runtest quicksort quicksort_positive1 "quicksort_positive1" 5;
runtest quicksort quicksort_positive2 "quicksort_positive2" 5;
runtest quicksort quicksort_positive3 "quicksort_positive3" 5;
runtest quicksort quicksort_random "quicksort_random" 5;
runtest quicksort quicksort_zeroandone "quicksort_zeroandone" 5;
runtest quicksort quicksort9 "quicksort9" 5;
runtest quicksort quicksort_1000input "quicksort_1000input" 5;
runtest quicksort quicksort_100input "quicksort_100input" 5;
runtest multiply multiply_bonus1 "multiply_bonus1" 5;
runtest multiply multiply_bonus2 "multiply_bonus2" 5;

echo > test.out
echo >> test.out
echo   "-------------------------------------------------" >> test.out
echo   "CS250: Lab 7. Recursion + Using the Stack. User:  $USER   " >> test.out
echo   "-------------------------------------------------" >> test.out
cat total.txt >> test.out
echo   "-------------------------------------------------" >> test.out
echo   "                              	    Total: " $total of 105 >> test.out
cat test.out
rm *.out

