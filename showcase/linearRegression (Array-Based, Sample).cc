#include <iostream>
#include <string>

using namespace std;
void simpleLinearRegression(int x[], int y[], int sizeX, int sizeY) {
	double avgX = 0;
	double avgY = 0;
	double sumX = 0;
	double sumY = 0;
	double cov = 0;
	double var = 0;
	for (int i = 0; i < sizeX; i++) {
		sumX = sumX + (double)x[i];
		sumY = sumY + (double)y[i];
	}
	avgX = sumX/(double)sizeX;
	avgY = sumY/(double)sizeY;
	for (int i = 0; i < sizeX; i++) {
		cov = cov + ((x[i]-avgX)*(y[i]-avgY));
		var = var + ((x[i]-avgX)*(x[i]-avgX));
	}
	double beta = cov/var;
	double alpha = avgY - beta * avgX;
	double f[sizeX];
	for (int i = 0; i < sizeX; i++) {
		f[i] = alpha + beta * x[i];
	}
	double ssres = 0;
	double sstot = 0;
	for (int i = 0; i < sizeX; i++) {
		ssres = ssres + ((y[i]-f[i])*(y[i]-f[i]));
		sstot = sstot + ((y[i]-avgY)*(y[i]-avgY));
	}
	double rsquare = 1 - ssres/sstot;
	cout << "The estimation expression is" << " y=" << beta << "*x+(" << alpha << ")." <<endl;
	cout << "The expression could explain" << " " << rsquare*100 << "%" << " of the variation in the original dataset." <<endl;
}

int main() {
  int x[10]={1,2,3,4,5,6,7,8,9,10};
  int y[10]={7,2,9,0,5,4,10,900,1,23};
  simpleLinearRegression(x,y,sizeof(x)/sizeof(int),sizeof(y)/sizeof(int));
}
