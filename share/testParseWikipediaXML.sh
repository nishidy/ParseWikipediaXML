#!/bin/bash -eu

[[ ${#@} -eq 3 ]] || exit 1

prog=$1
numTest=$2
numDoc=$3

[[ -d "test" ]] && { echo "Must have clean 'test' directory."; exit 2; }

mkdir -p test

bofwfile="test/bofw"
docfile="test/doc"
outfile="test/out"

for n in $(seq 0 $numTest);do
	echo "[Test #${n}]"

	nthbofwfile="$bofwfile$n.txt"
	nthdocfile="$docfile$n.txt"
	nthoutfile="$outfile$n.txt"
	
	#./makeTestBagofwords numDocs maxNumTermsInDocs maxNumCharOfTerm maxFreq

	echo "Make bag-of-words file $nthbofwfile."
	./makeTestBagofwords $numDoc 100 50 100 > $nthbofwfile

	echo "Make documents file $nthdocfile from $nthbofwfile."
	./makeTestDocuments $nthbofwfile > $nthdocfile

	echo "Run '$prog' to make bag-of-wrods file from $nthdocfile."
	$prog -i "$nthdocfile" -s "$nthoutfile" -d "morph_english.flat" -c 1 -b

	echo "Check diff betwen $nthbofwfile and $nthoutfile."
	if diff $nthbofwfile $nthoutfile > /dev/null; then
		echo "OK. '$prog' is stable."
	else
		echo "NG. '$prog' is not stable. Please check the program. Aborted."
		exit 1
	fi

done


