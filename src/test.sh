TESTFILES="../test/*.dave"
suffix=".dave"
prefix="../test/"
run=0
success=0

# run Makefile
make all

Compare() { #compare the two files
	diff -bq "$1" "$2" && {
	    (( success++ ))
	    echo "PASS"
	} || {
	    echo "FAILED: does not match expected output"
	}
}

TESTING GOOD CASES
echo "Testing good cases:"
for f in $TESTFILES
do
	(( run++ ))
	echo "###################"
	name=${f#$prefix}
	name=${name%$suffix}
	ideal="../test/ideal/$name.out" #set the expect output path 
	rm -f "../test/$name.out" #remove previous output
	echo "Testing: $name"
	./dave -c < "$f" 2>&1 && {
		g++ -w dave.cc -Isrc 2>&1 && {
			./a.out > "../test/$name.out"
			Compare "../test/$name.out" "../test/ideal"
		} || {
			cat "../test/$name.out"
			echo "FAILED: did not compile"
		}
	} || {
		cat "../test/$name.out"
		echo "FAILED: did not compile"
	}
done

echo "SUMMARY"
echo "Number of tests run: $run"
echo "Number Good Cases Passed: $success"
echo "######################################"


# TESTING BAD CASES
BADFILES="../test/bad/*.dave"
badrun=0;
fail=0;

echo "Testing bad cases:"
for f in $BADFILES
do
	(( badrun++ ))
	echo "###################"
	name=${f#$prefix}
	name=${name%$suffix}
	ideal="../test/ideal/$name.out" #set the expect output path 
	rm -f "../test/$name.out" #remove previous output
	echo "Testing: $name"
	./dave -c < "$f" 2>&1 && {
		g++ -w dave.cc -Isrc 2>&1 && {
			echo "FAILED: bad case successfully compiled"
		} || {
			cat "../test/$name.out"
			echo "PASS"
			(( fail++ ))
		}
	} || {
		cat "../test/$name.out"
		echo "PASS"
		(( fail++ ))
	}
done

echo "SUMMARY"
echo "Number of tests run: $badrun"
echo "Number of Bad Cases Passed: $fail"