#define __DAVE_H_

#include "dave.hpp"

void _print(int n) {
	cout << n << endl;
}

void _print(double d) {
	cout << d << endl;
}

void _print(string str) {
	cout << str << endl;
}

void _print(const char *str) {
	cout << str << endl;
}

void _print(bool b) {
	cout << (b ? "true" : "false") << endl;
}

void _print(rec r) {
	cout << r << endl;
}

void _print(fld f) {
	cout << f << endl;
}

void _print(tbl t) {
	cout << t << endl;
}

vector<int> _to_vector(int arr[], int length) {
	vector<int> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<double> _to_vector(double arr[], int length) {
	vector<double> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<bool> _to_vector(bool arr[], int length) {
	vector<bool> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<string> _to_vector(string arr[], int length) {
	vector<string> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<fld> _to_vector(fld arr[], int length) {
	vector<fld> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<rec> _to_vector(rec arr[], int length) {
	vector<rec> v;
	for (int i = 0; i < length; i++) {
		v.push_back(arr[i]);
	}
	return v;
}

vector<int> _slice_array(vector<int> src, int begin, int end) {
	int j = 0;
	for (int i = begin; i < end; i++) {
		src[j++] = src[i];
	}
	src.resize(end-begin);
	return src;
}

vector<double> _slice_array(vector<double> src, int begin, int end) {
	int j = 0;
	for (int i = begin; i < end; i++) {
		src[j++] = src[i];
	}
	src.resize(end-begin);
	return src;
}

vector<string> _slice_array(vector<string> src, int begin, int end) {
	int j = 0;
	for (int i = begin; i < end; i++) {
		src[j++] = src[i];
	}
	src.resize(end-begin);
	return src;
}

vector<bool> _slice_array(vector<bool> src, int begin, int end) {
	int j = 0;
	for (int i = begin; i < end; i++) {
		src[j++] = src[i];
	}
	src.resize(end-begin);
	return src;
}

vector<fld> _slice_array(vector<fld> src, int begin, int end) {
	vector<fld> dest;
	int j = 0;
	for (int i = begin; i < end; i++) {
		dest.push_back(src[i]);
	}
	return dest;
}

vector<rec> _slice_array(vector<rec> src, int begin, int end) {
	vector<rec> dest;
	int j = 0;
	for (int i = begin; i < end; i++) {
		dest.push_back(src[i]);
	}
	return dest;
}
