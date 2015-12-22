#include <iostream>
#include <string>

using namespace std;
void copyArray(int B[], int A[], int n)
{
    for(int i = 0; i < n; i++)
        A[i] = B[i];
}

void bottomUpMerge(int A[], int iLeft, int iRight, int iEnd, int B[]) {
    int i = iLeft;
    int j = iRight;
    for (int k = iLeft; k < iEnd; k++) {
        if (i < iRight && (j >= iEnd || A[i] <= A[j])) {
            B[k] = A[i];
            i = i + 1;
        } else {
            B[k] = A[j];
            j = j + 1;    
        }
    } 
}

void mergeSort (int A[], int B[], int n) {
  for (int width = 1; width < n; width = 2 * width) {
      for (int i = 0; i < n; i = i + 2 * width) {
          bottomUpMerge(A, i, min(i+width, n), min(i+2*width, n), B);
      }
      copyArray(B,A,n);
  }
}

int main() {
  int A[4]={7,4,3,2};
  int B[4]={0,0,0,0};
  mergeSort(A,B,4);
  cout<<A[0];
  cout<<A[1];
  cout<<A[2];
  cout<<A[3];
}