#define __DAVE_H_

#include "dave.hpp"

void print(int n) {
	cout << n << endl;
}

void print(double d) {
	cout << d << endl;
}

void print(string str) {
	cout << str << endl;
}

void print(const char *str) {
	cout << str << endl;
}

void print(bool b) {
	cout << (b ? "true" : "false") << endl;
}

void print(rec r) {
	cout << r << endl;
}

void print(fld f) {
	cout << f << endl;
}

void print(tbl t) {
	cout << t << endl;
}

int get_int(fld a, int index) {
	_print(a.f_int[index]);
	return a.f_int[index];
}

double get_float(fld a, int index) {
	return a.f_double[index];
}

string get_string(fld a, int index) {
	return a.f_string[index];
}

bool get_bool(fld a, int index) {
	return a.f_bool[index];
}

vector<int> to_vector(int arr[], int length) {
	vector<int> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<double> to_vector(double arr[], int length) {
	vector<double> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<bool> to_vector(bool arr[], int length) {
	vector<bool> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<string> to_vector(string arr[], int length) {
	vector<string> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<fld> to_vector(fld arr[], int length) {
	vector<fld> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<rec> to_vector(rec arr[], int length) {
	vector<rec> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<int> slice_array(vector<int> src, int begin, int end) {
	int j = 0;
	for (int i = begin; i < end; i++) {
		src[j++] = src[i];
	}
	src.resize(end-begin);
	return src;
}

vector<double> slice_array(vector<double> src, int begin, int end) {
	int j = 0;
	for (int i = begin; i < end; i++) {
		src[j++] = src[i];
	}
	src.resize(end-begin);
	return src;
}

vector<string> slice_array(vector<string> src, int begin, int end) {
	int j = 0;
	for (int i = begin; i < end; i++) {
		src[j++] = src[i];
	}
	src.resize(end-begin);
	return src;
}

vector<bool> slice_array(vector<bool> src, int begin, int end) {
	int j = 0;
	for (int i = begin; i < end; i++) {
		src[j++] = src[i];
	}
	src.resize(end-begin);
	return src;
}

vector<fld> slice_array(vector<fld> src, int begin, int end) {
	vector<fld> dest;
	int j = 0;
	for (int i = begin; i < end; i++) {
		dest.push_back(src[i]);
	}
	return dest;
}

vector<rec> slice_array(vector<rec> src, int begin, int end) {
	vector<rec> dest;
	int j = 0;
	for (int i = begin; i < end; i++) {
		dest.push_back(src[i]);
	}
	return dest;
}
