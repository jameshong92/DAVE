# DAVE - Data Analysis Made Very Easy
## Programming Languages and Translators Project
* James HyunSeung Hong (hh2473)
* Min Woo Kim (mk3351)
* Fan Yang (fy2207)
* Chen Yu (cy2415)

## 1. Introduction
The DAVE language is a programming language optimized for data retrieval, manipulation, and analysis. We designed DAVE with cross-dataset operations in mind, which are frequently needed but complicated to implement with existing technologies in today’s data-intensive environments. Operators would be able to use DAVE to validate datasets, incorporate (parts of) datasets from different sources, split up oversized datasets, and conduct statistical analysis.
### 1.1 Motivation
Over the last few decades developers have created a variety of tools to help with data management and analysis. SQL and R language, for example, are two of the most prominent tools in this field, being widely utilized by business analysts, social scientists, statisticians, and many more.
Popular as they are, there still remain certain scenarios where both tools are imperfect. For instance, social scientists often need to incorporate data of distinct formations from different sources for their statistical analysis. SQL may work well for this job, yet it is designed for relational databases, a system which is not suitable for most statistical tasks. R language, on the other hand, is fully optimized for statistical analysis with all the functions and libraries, but it lacks the simplicity and usability of SQL in terms of managing data.
We have built DAVE to address this problem. DAVE is an integration rather than combination of the two prevailing languages, utilizing their advantages to build up a new world.
### 1.2 Target
The syntax of DAVE language is JAVA-like and simple enough for beginners to learn. The most unique and useful feature of DAVE is its three built-in data structures: fld, rec, and tbl, which represent a column of a table, a row of a table, and a table of data, respectively. These DAVE data structures will be particularly useful for cross-dataset operations. In addition, DAVE includes some built-in statistical functions to analyze the data of interest. By providing a generic tool that can be applied widely, DAVE envisions to develop a large open source community that is beneficial to all.
## 2 Language Tutorial
### 2.1 How the Compiler Works
#### 2.1.1 Environment Setup
DAVE language needs the GNU Compiler Collection 5 (GCC5) or later version to function, as it compiles to C++. The included helper scripts run only in an environment with Bash, and the make command would be useful when compiling your DAVE source code files. However, neither Bash nor make is required to run DAVE.
We recommend you to:
*	Locate the DAVE Standard Library files (dave.h, dave.hpp, dave_io.hpp) under the same directory of your DAVE source codes
*	Add the DAVE binaries to your PATH environment variable

#### 2.1.2 Compiling and Running DAVE
The standard DAVE Package comes with the following source code files (under /src folder):
```
ast.ml	compile.ml
dave_io.hpp	dave.hpp
dave.ml	dave.h
dave_core.dave	Makefile
Makefile.ocaml	parser.mly
sast.ml	scanner.ml
emanticAnalysis.ml	semanticExceptions.ml
```

You need to compile the provided source code files with DAVE compiler first before compiling and running any DAVE program. In order to make the DAVE compiler, run the following commands:
```
make
```
Suppose the source code file of your DAVE program is named my_program.dave, the following commands would compile your DAVE program to executable:
```
./dave -c <my_program.dave
g++ dave.cc
```
Alternatively, you can compile with the friendly shell script with the following command:
```
./ezcompile.sh
```
It is important to note that Makefile, generated compiler, and ezcompile.sh are all under /src directory. Therefore, the above command will work if you are currently under /src directory.
### 2.2 Remarkable Features
As mentioned in the introduction, the most remarkable feature of DAVE is that it provides the data structures representing a record (rec), a field (fld), and a table (tbl).
fld contains a sequential collection of homogeneous variables. It is similar to an array data structure in other languages, but fld contains a label that can be used to easily retrieve or modify the data of your interest. Because its elements are homogeneous variables, most of the arithmetic operations can be applied to some parts or the whole of the dataset.
rcd contains a sequential collection of heterogeneous variables. Each tuple-like rcd variable contains a name and the corresponding contents. Each content variable can be of int, double, bool or string data types. Typically, a row of the table can be translated into a rcd. Arithmetic operations may be difficult to be applied here, but it would be useful for applying querying functions.
tbl represents a data table that consists of one or more flds or recs. Nearly every SQL-like dataset can be translated into a tbl variable. Every tbl can be decomposed into one or more flds or recs. It is particularly useful for the cross-functional data analytics process. As we provided the functionalities for users to directly generate a tbl variable from the given dataset, it would be very convenient for the user to handle multiple datasets simultaneously, incorporate the required information, apply analysis, and write the result back.
