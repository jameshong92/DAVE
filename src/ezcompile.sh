echo "Enter the Filename of Your DAVE Source Code File:"
read filename
echo "Option 1: Compile Your DAVE Source Code to C++ Source Code"
echo "Option 2: Compile Your DAVE Source Code to Executable"
read -p "[1,2]?:" input
if [ "$input" == "1" ]; then
	./dave -c <$filename
	if [ $? != 0 ]; then
		echo "Failed to Compile to C++ Source Code."
		exit 1
	fi
	echo "$filename.dave Has Been Compiled to C++ Source Code (dave.cc)."
fi
if [ "$input" == "2" ]; then
	./dave -c <$filename
	if [ $? != 0 ]; then
		echo "Failed to Compile to Executable (Unable to Compile to C++ Source Code with DAVE Compiler)."
		exit 1
	fi
	g++ -w dave.cc
	if [ $? != 0 ]; then
	echo "Failed to Compile to Executable (Unable to Compile to Executable with g++ Compiler)."
	exit 1
	fi
	echo "$1 Compiled to Executable (a.out)."
fi
if [ "$input" != "1" ] && [ "$input" != "2" ] ; then
	echo "Invaild Input."
	exit 1
fi

