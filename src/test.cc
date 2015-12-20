#include "dave.h"
#include "dave.hpp"
using namespace std;

int main(int argc, char const *argv[]) {
    int a[] = {90,99,98};
    string g[]  = {"ab", "cd", "ef"};
    fld b = fld (a , "value", getArrayLen(a));
    // sample definition of fld
    fld h = fld (g, "word", getArrayLen(g));
    cout << b;
    cout << h;
    tuple e[] = {tuple (22, "age"), tuple ("Fan", "name")};
    rec f = rec (e, getArrayLen(e));
    // sample definition of rec
    cout << f;
    tuple j[] = {tuple (20, "age"), tuple ("James", "name")};
    rec k = rec (j, getArrayLen(j));
    tuple u[] = {tuple (21, "age"), tuple ("Min", "name")};
    rec v = rec (u, getArrayLen(u));
    cout << k;
    fld i[] = {b,h};
    rec l[] = {f,k,v};
    tbl t = tbl(i, i[0].length, getArrayLen(i));
    // sample definition of tbl (kind 1)
    cout << t;
    tbl s = tbl(l, getArrayLen(l), l[0].length);
    // sample definition of tbl (kind 2)
    cout << s;
    tbl n = tbl(s, b);
    // sample definition of binding (kind 1)
    cout << "result of binding1 is:" << endl;
    cout << n;
    tuple m[] = {tuple (21, "age"), tuple ("Micheal", "name")};
    tuple y[] = {tuple (48, "age"), tuple ("Edwards", "name")};
    rec w = rec (m, getArrayLen(m));
    rec p = rec (y, getArrayLen(m));
    rec q[] = {w,p};
    tbl x = tbl(q, getArrayLen(q), q[0].length);
    cout << x;
    tbl z = tbl(s, w);
    // sample definition of binding (kind 2)
    cout << "result of binding2 is:" << endl;
    cout << z;
    tbl st = tbl(s, t, true);
    // sample definition of binding (kind 3)
    cout << "result of binding3 is:" << endl;
    cout << st;
    tbl sx = tbl(s, x, false);
    // sample definition of binding (kind 4)
    cout << "result of binding4 is:" << endl;
    cout << sx;
    tbl ss = tbl(sx, 1, 2, "Cesc");
    // sample definition of changing (kind 4)
    cout << "result of changing is:" << endl;
    cout << ss;
    tbl sx1 = tbl(sx, 1, "string");
    cout << "result of conversion is:" << endl;
    cout << sx1;
    tbl sx2 = tbl(sx1, 1, "int");
    cout << "result of conversion is:" << endl;
    cout << sx2;
    // sample of data type conversion
    return 0;
}
