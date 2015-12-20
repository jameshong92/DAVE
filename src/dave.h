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

void slice_array(int *src, int *dest, int begin, int end) {
	for (int i = begin; i < end; i++) {
		*dest++ = src[i];
	}
}

int * slice_array(int *src, int begin, int end) {
	int dest[end - begin];
	int a = 0;
	for (int i = begin; i < end; i++) {
		dest[a] = src[i];
		a += 1;
	}
	return dest;
}

void slice_array(float *src, float *dest, int begin, int end) {
	for (int i = begin; i < end; i++) {
		*dest++ = src[i];
	}
}

void slice_array(bool *src, bool *dest, int begin, int end) {
	for (int i = begin; i < end; i++) {
		*dest++ = src[i];
	}
}

#endif
