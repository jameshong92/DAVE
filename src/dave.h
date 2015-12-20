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

int* slice_array(int *arr, int size, int i1, int i2) {
	i2 = i2 == -1 ? size/sizeof(*arr) : i2;
	int dest[i2-i1];
	int j = 0;
	for (int i = i1; i < i2; i++) {
		dest[j++] = arr[i];
	}
	return dest;
}

float* slice_array(float *arr, int size, int i1, int i2) {
	i2 = i2 == -1 ? size/sizeof(*arr) : i2;
	float dest[i2-i1];
	int j = 0;
	for (int i = i1; i < i2; i++) {
		dest[j++] = arr[i];
	}
	return dest;
}

bool* slice_array(bool *arr, int size, int i1, int i2) {
	i2 = i2 == -1 ? size/sizeof(*arr) : i2;
	bool dest[i2-i1];
	int j = 0;
	for (int i = i1; i < i2; i++) {
		dest[j++] = arr[i];
	}
	return dest;
}

#endif
