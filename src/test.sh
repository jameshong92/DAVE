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
	    echo "FAILDED: does not match expected output"
	}
}

for f in $TESTFILES
do
	(( run++ ))
	echo "file: $f"
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
			echo "FAILDED: did not compile"
		}
	} || {
		cat "../test/$name.out"
		echo "FAILDED: did not compile"
	}
done

echo "SUMMARY"
echo "Number of tests run: $run"
echo "Number Passed: $success"
