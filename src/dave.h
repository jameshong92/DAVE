#ifndef __DAVE_H_
#define __DAVE_H_

#include <iostream>
#include <string>

using namespace std;

void print(int n) {
	cout << n << endl;
}

void print(float n) {
	cout << n << endl;
}

void print(string n) {
	cout << n << endl;
}

void print(const char *n) {
	cout << string(n) << endl;
}

void print(bool v) {
	cout << (v ? "true" : "false") << endl;
}

#endif