#ifndef _DAVE_HPP_
#define _DAVE_HPP_
#include <iostream>
#include <vector>
#include <string>
#include <stdexcept>
#include <sstream>
#include <fstream>

using namespace std;
template <class T>

int getArrayLen(T &array) {
	return (sizeof(array)/sizeof(array[0]));
}

class tuple {
public:
    string name; 
    T content;
	tuple(T ,string);
}

class rec {
public:
	int length;
	vector<tuple> m; 
	rec(tuple *);
}

class fld {
public: 
    int length;
    string name;
    vector<T> m;
    fld(T *, string);
}

class tbl {
public:
	int row_length;
    int col_length;
    vector<fld> m;
    tbl(fld *);
    tbl(tbl *);
} 

tuple::tuple(T val; string str) {
	name = str;
	content = val;
}

fld::fld(T *array; string str) {
    length = getArrayLen(array);
    name = str;
    for (int i=0; i<length; i++)
    	m.push_back(array[i]);
}

rec::rec(tuple *array) {
    length = getArrayLen(array);
    for (int i=0; i<length; i++)
    	m.push_back(array[i]);
}

tbl::tbl(fld *array) {
    row_length = getArrayLen(array);
    col_length = getArrayLen(array[0]);
    for (int i=0; i<length; i++) 
    	m.push_back(array[i]);
}

ostream & operator << (ostream &sys, const rec &in) {
	for (int i=0; i<in.length; i++) {
		sys << in.m[i].name;
		if (i<in.length-1) {
		    sys << "\t";
		}
	} 
	sys << endl << "[";
	for (int i=0; i<in.length; i++) {
		sys << in.m[i].content;
		if (i<in.length-1) {
			sys << ",\t";
		}
	}
	sys << "]" << endl;
	return sys;
}

ostream & operator << (ostream &sys, const fld &in) {
	sys << fld.name << endl;
	for (int i=0; i<in.length; i++) {
		sys << in.m[i] << endl;
	}
	return sys;
}

ostream & operator << (ostream &sys, const tbl &in) {
    for (int i=0; i<in.row_length; i++) {
        if (i<in.row_length-1) {
		    sys << in.m[i].name << "\t";
		}
    }
    for (int i=0; i<in.row_length; i++) {
    	sys << "[";
    	for (int j=0; j<in.col_length; j++) {
    		sys << in.m[i].m[j];
    		if (j < in.col_length-1) {
    			sys << ",\t";
    		}
    	}
    	sys << "]" << endl;
    }
    return sys;
}
