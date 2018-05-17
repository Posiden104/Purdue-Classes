#!/bin/bash
#Password Strength Checker

SCORE=0



pass=$1
help_=0

if [ $# -eq 2 ]; then
	echo Password helper enabled
	let help_=1
fi 

if [ $# -lt 1 ]; then
	echo "Please supply a password to check."
	exit
fi

if [ ${#pass} -lt 6 ] || [ ${#pass} -gt 32 ]; then
	echo "Error: Password length invalid."
	exit
fi

passlen=${#pass}

let SCORE=SCORE+$passlen

if egrep -q [#$\+%@] <<< "$pass" ; then 
	let SCORE=SCORE+5	
else
	if [ $help_ -eq 1 ]; then
		echo Try adding a special character to strengthen password
	fi
fi

if egrep -q [0-9] <<< "$pass" ; then
	let SCORE=SCORE+5
else 
	if [ $help_ -eq 1 ]; then
		echo Try adding a number to strengthen password
	fi
fi

if egrep -q [A-Za-z] <<< "$pass" ; then
	let SCORE=SCORE+5
else
	if [ $help_ -eq 1 ]; then
		echo Try adding a letter to strengthen password
	fi
fi

if egrep -q "([A-Za-z])\1+" <<< "$pass" ; then
	let SCORE=SCORE-10
	if [ $help_ -eq 1 ]; then
		echo Try removing repeated letters to strengthen password
	fi
fi

if egrep -q [a-z][a-z][a-z] <<< "$pass" ; then
	let SCORE=SCORE-3
	if [ $help_ -eq 1 ]; then
		echo Try removing 3+ lowercase letters in a row to strengthen password
	fi
fi

if egrep -q [A-Z][A-Z][A-Z] <<< "$pass" ; then
	let SCORE=SCORE-3
	if [ $help_ -eq 1 ]; then
		echo Try removing 3+ uppercase letters in a row to strengthen password
	fi
fi

if egrep -q [0-9][0-9][0-9] <<< "$pass" ; then
	let SCORE=SCORE-3
	if [ $help_ -eq 1 ]; then
		echo Try removing 3+ numbers in a row to strengthen password
	fi
fi

echo "Password Score:" $SCORE
exit
