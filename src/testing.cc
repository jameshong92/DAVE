#include "dave.h"

int main(int argc, char const *argv[]) {

  int intarray[] = {1, 2, 3};
  vector<int> intvector = to_vector(intarray, sizeof(intarray));
  print(intvector[1]);

  vector<double> doublevector(5);
  doublevector[0] = 0.1;
  doublevector[1] = 0.2;
  doublevector[2] = 0.3;
  doublevector[3] = 0.4;
  doublevector[4] = 0.5;

  vector<double> destdoublevector = slice_array(doublevector, 1, 5);
  print(destdoublevector[1]);
  print(slice_array(doublevector, 1, 5)[1]);

  vector<string> stringvector(5);
  stringvector[0] = "a";
  stringvector[1] = "b";
  stringvector[2] = "c";
  stringvector[3] = "d";
  stringvector[4] = "e";

  vector<string> deststringvector = slice_array(stringvector, 1, 5);
  print(deststringvector[1]);
  print(slice_array(stringvector, 1, 5)[1]);

  int a[] = {90,99,98};
  string g[]  = {"ab", "cd", "ef"};
  fld b = fld (a , "value", getArrayLen(a));
  print(b);

  vector<fld> fldvector;
  fldvector.push_back(b);
  fldvector.push_back(b);
  fldvector.push_back(b);
  print(slice_array(fldvector,1, 3)[1]);

  vector<fld> newfldvector = slice_array(fldvector, 1, 3);
  for (int i = 0; i < newfldvector.size(); i++) {
    print("New Fld vector: ");
    print(newfldvector[i]);
  }

  return 0;
}
