#include "dave.h"

int max(fld a) {
{
int max;
int current = get_int(a, 0);
int length = (a.length);
int i;
for ((i = 1); (i < length); (i++)) {
if ((get_int(a, i) < current))
{
((max = current));
}

}

return max;
}

}
int min(fld a) {
{
int min;
int i;
int length = (a.length);
int current = get_int(a, 0);
for ((i = 1); (i < length); (i++)) {
if ((get_int(a, i) > current))
{
((min = current));
}

}

return min;
}

}

int min(int a, int b) {
{
if ((a >= b))
{
return b;
}
else {
return a;
}

}

}
void cpyArr(vector<int> B, vector<int> A, int n) {
{
int i = 0;
for ((i = 0); (i < n); (i++)) {
(((A[i]) = (B[i])));
}

}

}
void bupMerge(vector<int> A, int left, int right, int end, vector<int> B) {
{
int i = left;
int j = right;
int k = 0;
for ((k = left); (k < end); (k++)) {
if (((i < right) && ((j >= end) || ((A[i]) <= (A[j])))))
{
(((B[k]) = (A[i])));
((i = (i + 1)));
}
else {
(((B[k]) = (A[j])));
((j = (j + 1)));
}

}

}

}
void mergeSort(vector<int> A, vector<int> B, int n) {
{
int w = 1;
int i = 0;
for ((w = 1); (w < n); (w = (2 * w))) {
for ((i = 0); (i < n); (i = (i + (2 * w)))) {
(bupMerge(A, i, min((i + w), n), min((i + (2 * w)), n), B));
}

(cpyArr(B, A, n));
}

}

}
int main() {
{
int _A[] = {6, 9, 3, 10};
vector<int> A = to_vector(_A, getArrayLen(_A));
int _B[] = {0, 0, 0, 0};
vector<int> B = to_vector(_B, getArrayLen(_B));
(print("Before"));
(print((A[0])));
(print((A[1])));
(print((A[2])));
(print((A[3])));
(mergeSort(A, B, 4));
(print("After"));
(print((A[0])));
(print((A[1])));
(print((A[2])));
(print((A[3])));
}

}
