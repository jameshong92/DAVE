TESTFILES="dave_test/*.dave"
run=0
success=0

Compare() { #compare the two files
	diff -bq "$1" "$2" && {
	    (( success++ ))
	    echo "PASS"
	} || {
	    cat "$1"
	    echo "FAILDED: does not match expected output"
	}
}

for f in $ TESTFILES
do
	(( run++ ))
	name = ${f%.dave}  #remove .dave suffix
	name = ${name#dave_test/}  #remove dave_test/ prefix
	ideal = ${f%name.dave}"ideal/$name.out" #set the expect output path 
	echo "Testing: $name"
	./dave "$f" > "dave_test/name.out" 2>&1 && {
	    Compare "dave_test/$name.out" "ideal"
	} || {
		cat "dave_test/$name.out"
		echo "FAILDED: did not compile"
	}
done

echo "SUMMARY"
echo "Number of tests run: $run"
echo "Number Passed: $success"
