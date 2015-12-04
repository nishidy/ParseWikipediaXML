#!/bin/bash -eu

[[ ${#@} -eq 4 ]] || { echo "$0 \"prog\" numTests numWorkers numDocs"; exit 1; }

prog=$1
numTests=$2
numWorkers=$3

numDocs=$4
maxNumTermsInDocs=1000
maxNumCharOfTerm=20
maxFreqOfTerm=100

[[ -d "test" ]] && {
	echo "Must have clean 'test' directory."
	echo -n "If you want to destroy it, enter 'Go!'. : "
	read input
	if [[ $input == "Go!" ]]; then
		rm -rf test
		[[ -d "test" ]] && { echo "Could not destroy it..."; exit 3; }
	else
		echo "Bye."
		exit 2
	fi
}

mkdir -p test

bofwfile="test/bofw"
docfile="test/doc"
outfile="test/out"

for n in $(seq 1 $numTests);do
	echo "[Test #${n}]"

	nthbofwfile="$bofwfile$n.txt"
	nthdocfile="$docfile$n.txt"
	nthoutfile="$outfile$n.txt"
	
	#./makeTestBagofwords numDocs maxNumTermsInDocs maxNumCharOfTerm maxFreq

	echo "Make bag-of-words file $nthbofwfile."
	./makeTestBagofwords $numDocs $maxNumTermsInDocs $maxNumCharOfTerm $maxFreqOfTerm > $nthbofwfile

	echo "Make documents file $nthdocfile from $nthbofwfile."
	./makeTestDocuments $nthbofwfile > $nthdocfile

	echo "Run '$prog' to make bag-of-wrods file from $nthdocfile."
	$prog -i "$nthdocfile" -s "$nthoutfile" -c 1 -w $numWorkers

	echo "Check diff betwen $nthbofwfile and $nthoutfile."
	if ./checkDiffMultilines $nthbofwfile $nthoutfile; then
		echo "OK. '$prog' is stable."
	else
		echo "NG. '$prog' is not stable. Please check the program. Aborted."
		exit 1
	fi

done


