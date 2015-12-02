/* #ifndef _DAVE_HPP_
#define _DAVE_HPP_ */
#include <iostream>
#include <vector>
#include <string>
#include <stdexcept>

using namespace std;

template <class T>
int getArrayLen(T& array)
{
    return (sizeof(array)/sizeof(array[0]));
}

class tuple {
public:
    string name; 
    int type;
    int content_int;
    double content_double;
    string content_string;
	tuple(int initial,string str) {
        type = 0;
        content_int = initial;
        name = str;
    }
    tuple(double initial,string str) {
        type = 1;
        content_double = initial;
        name = str;
    }
    tuple(string initial,string str) {
        type = 2;
        content_string = initial;
        name = str;
    }
};

class rec {
public:
	int length;
	vector< tuple > r;
    rec(tuple *array, int len) {
        length = len;
        for (int i=0; i<length; i++)
            r.push_back(array[i]);
    }
}; 

class fld {
public: 
    int length;
    string name;
    int type;
    vector<int> f_int;;
    vector<double> f_double;
    vector<string> f_string;  
    fld(int *array, string str, int len) {
        type = 0;
        length = len;
        name = str;
        for (int i=0; i<length; i++)
            f_int.push_back(array[i]);
    }
    fld(double *array, string str, int len) {
        type = 1;
        length = len;
        name = str;
        for (int i=0; i<length; i++)
            f_double.push_back(array[i]);
    }
    fld(string *array, string str, int len) {
        type = 2;
        length = len;
        name = str;
        for (int i=0; i<length; i++)
            f_string.push_back(array[i]);
    }
};


class tbl {
public:
	int row_length;
    int col_length;
    vector<fld> t;
    tbl(fld *array, int col_len, int row_len) {
        col_length = col_len;
        row_length = row_len;
        for (int i=0; i<row_len; i++) {
            t.push_back(array[i]);
        }
    }
    tbl(rec *array, int col_len, int row_len) {
        col_length = col_len;
        row_length = row_len;
        for (int i=0; i<row_len; i++) {
            string name = array[0].r[i].name;
            if (array[0].r[i].type == 0) {
                int data[col_len];
                for (int j=0; j<col_len; j++) {
                    data[j] = array[j].r[i].content_int;
                }
                fld temp = fld (data, name, col_len);
                t.push_back(temp);
            } else if (array[0].r[i].type == 1) {
                double data[col_len];
                for (int j=0; j<col_len; j++) {
                    data[j] = array[j].r[i].content_double; 
                }
                fld temp = fld (data, name, col_len);
                t.push_back(temp);
            } else if (array[0].r[i].type == 2) {
                string *data = new string[col_len];
                for (int j=0; j<col_len; j++) {
                    data[j] = array[j].r[i].content_string;
                }
                fld temp = fld (data, name, col_len);
                t.push_back(temp);
            }
        }
    }
    tbl(tbl original, fld newfld) {
        col_length = original.col_length;
        row_length = original.row_length + 1;
        t = original.t;
        t.push_back(newfld);
    }
    tbl(tbl original, rec newrec) {
        col_length = original.col_length + 1;
        row_length = original.row_length;
        t = original.t;
        for (int i=0; i<newrec.length; i++) {
            for (int j=0; j<newrec.length; j++) {
                if (newrec.r[i].name.compare(t[j].name) == 0) {
                    if (newrec.r[i].type == 0) {
                        t[j].f_int.push_back(newrec.r[i].content_int);
                    } else if (newrec.r[i].type == 1) {
                        t[j].f_double.push_back(newrec.r[i].content_double);
                    } else if (newrec.r[i].type == 2) {
                        t[j].f_string.push_back(newrec.r[i].content_string);
                    } 
                    break;
                }
            }
        }
    }
}; 

tbl plus(tbl source1, tbl source2) {
    if (source1.col_length != source2.col_length ||
        source1.row_length != source2.row_length) {
        return source1;
    }
    for (int i=0; i<source1.col_length; i++) {
        for (int j=0; j<source1.row_length; j++) {
            if (source1.t[j].type == 0 && source2.t[j].type == 0) {
                source1.t[j].f_int[i] += source2.t[j].f_int[i];
            }
            else if (source1.t[j].type == 1 && source2.t[j].type == 1) {
                source1.t[j].f_double[i] += source2.t[j].f_double[i];
            }
        }
    }
    return source1;
}

tbl minus(tbl source1, tbl source2) {
    if (source1.col_length != source2.col_length ||
        source1.row_length != source2.row_length) {
        return source1;
    }
    for (int i=0; i<source1.col_length; i++) {
        for (int j=0; j<source1.row_length; j++) {
            if (source1.t[j].type == 0 && source2.t[j].type == 0) {
                source1.t[j].f_int[i] -= source2.t[j].f_int[i];
            }
            else if (source1.t[j].type == 1 && source2.t[j].type == 1) {
                source1.t[j].f_double[i] -= source2.t[j].f_double[i];
            }
        }
    }
    return source1;
}


ostream & operator << (ostream & sys, const tuple &in) {
    sys << in.name << endl;
    if (in.type == 0) {
        sys << in.content_int;
    } else if (in.type == 1) {
        sys << in.content_double;
    } else if (in.type == 2) {
        sys << in.content_string;
    }
    sys << endl;
    return sys;
}

ostream & operator << (ostream &sys, const rec &in) {
	for (int i=0; i<in.length; i++) {
		sys << in.r[i].name;
		if (i<in.length-1) {
		    sys << "\t";
		}
	} 
	sys << endl << "[";
	for (int i=0; i<in.length; i++) {
        if (in.r[i].type == 0) {
            sys << in.r[i].content_int;
        } else if (in.r[i].type == 1) {
            sys << in.r[i].content_double;
        } else if (in.r[i].type == 2) {
            sys << in.r[i].content_string;
        }
		if (i<in.length-1) {
			sys << ",\t";
		}
	}
	sys << "]" << endl;
	return sys;
}

ostream & operator << (ostream &sys, const fld &in) {
	sys << in.name << endl;
    if ( in.type == 0 ) {
	    for (int i=0; i<in.length; i++)
		    sys << in.f_int[i] << endl;
    } else if ( in.type == 1 ) {
        for (int i=0; i<in.length; i++)
            sys << in.f_double[i] << endl;
    } else if ( in.type == 2 ) {
        for (int i=0; i<in.length; i++)
            sys << in.f_string[i] << endl; 
    }
	return sys;
}

ostream & operator << (ostream &sys, const tbl &in) {
    for (int i=0; i<in.row_length; i++) {
        if (i<in.row_length-1) {
		    sys << in.t[i].name << "\t";
		} else {
            sys << in.t[i].name << endl;
        }
    }
    for (int i=0; i<in.col_length; i++) {
    	sys << "[";
        for (int j=0; j<in.row_length; j++) {
            if ( in.t[j].type == 0 ) {
                sys << in.t[j].f_int[i];
                if (j < in.row_length-1) {
                    sys << ",\t";
                }
            } else if ( in.t[j].type == 1 ) {
                sys << in.t[j].f_double[i];
                if (j < in.row_length-1) {
                    sys << ",\t";
                }
            } else if ( in.t[j].type == 2 ) {
                sys << in.t[j].f_string[i];
                if (j < in.row_length-1) {
                    sys << ",\t";
                }
            }
        }
    	sys << "]" << endl;
    }
    return sys;
}

/*
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
    cout << n;
    tuple m[] = {tuple (21, "age"), tuple ("Micheal", "name"), tuple ("99.99","value")};
    rec w = rec (m, getArrayLen(m));
    s = tbl(s, w);
    // sample definition of binding (kind 2)
    cout << s;
    return 0; 
} */
