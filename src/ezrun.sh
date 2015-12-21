echo "Enter the Filename of Your DAVE Source Code File:"
read filename
./dave -c <$filename
if [ $? != 0 ]; then
	echo "Failed to Compile to C++ Source Code."
	exit 1
fi
./dave -c <$filename
if [ $? != 0 ]; then
	echo "Failed to Compile to Executable (Unable to Compile to C++ Source Code with DAVE Compiler)."
	exit 1
fi
g++ dave.cc
if [ $? != 0 ]; then
	echo "Failed to Compile to Executable (Unable to Compile to Executable with g++ Compiler)."
	exit 1
fi
./a.out

