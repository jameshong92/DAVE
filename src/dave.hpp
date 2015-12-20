/* #ifndef _DAVE_HPP_*/
#define _DAVE_HPP_ 
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
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
                vector<int> data;
                for(int j=0; j<col_len; j++) {
                    for (int k=0; k<row_len; k++) {
                        if (array[j].r[k].name.compare(name) == 0) {
                            data.push_back(array[j].r[k].content_int);
                        }
                    }
                }
                fld temp = fld (&data[0], name, col_len);
                t.push_back(temp);
            } else if (array[0].r[i].type == 1) {
                vector<double> data;
                for(int j=0; j<col_len; j++) {
                    for (int k=0; k<row_len; k++) {
                        if (array[j].r[k].name.compare(name) == 0) {
                            data.push_back(array[j].r[k].content_double);
                        }
                    }
                }
                fld temp = fld (&data[0], name, col_len);
                t.push_back(temp);
            } else if (array[0].r[i].type == 2) {
                vector<string> data;
                for(int j=0; j<col_len; j++) {
                    for (int k=0; k<row_len; k++) {
                        if (array[j].r[k].name.compare(name) == 0) {
                            data.push_back(array[j].r[k].content_string);
                        }
                    }
                }
                fld temp = fld (&data[0], name, col_len);
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
    tbl(tbl source1, tbl source2, bool ishorizonal) {
        if (ishorizonal) {
            if (source1.col_length != source2.col_length) {
                cout << "combine failed!" << endl;
                exit(1);
            }
            col_length = source1.col_length;
            row_length = source1.row_length + source2.row_length;
            t = source1.t;
            for (int i=0; i<source2.row_length; i++) {
                t.push_back(source2.t[i]);
            }
        } else {
            if (source1.row_length != source2.row_length) {
                cout << "combine failed!" << endl;
                exit(1);
            }
            col_length = source1.col_length + source2.col_length;
            row_length = source1.row_length;
            t = source1.t;
            for (int i=0; i<source2.row_length; i++) {
                for (int j=0; j<source1.row_length; j++) {
                    if (source2.t[i].name.compare(t[j].name) == 0) {
                        for (int k=0; k< source2.t[i].length; k++) {
                            if (source2.t[i].type == 0) {
                                t[j].f_int.push_back(source2.t[i].f_int[k]);
                            } else if (source2.t[i].type == 1) {
                                t[j].f_double.push_back(source2.t[i].f_double[k]);
                            } else if (source2.t[i].type == 2) {
                                t[j].f_string.push_back(source2.t[i].f_string[k]); 
                            }
                        }
                        break;
                    }
                }
            }
        }
    }
    tbl(tbl original, int col, int row, int newele) {
        if (col > original.col_length || row > original.row_length) {
            cout << "reset failed!" << endl;
            exit(1);
        }
        col_length = original.col_length;
        row_length = original.row_length;
        t = original.t;
        t[row-1].f_int[col-1] = newele;
    }
    tbl(tbl original, int col, int row, double newele) {
        if (col > original.col_length || row > original.row_length) {
            cout << "reset failed!" << endl;
            exit(1);
        }
        col_length = original.col_length;
        row_length = original.row_length;
        t = original.t;
        t[row-1].f_double[col-1] = newele;
    }
    tbl(tbl original, int col, int row, string newele) {
        if (col > original.col_length || row > original.row_length) {
            cout << "reset failed!" << endl;
            exit(1);
        }
        col_length = original.col_length;
        row_length = original.row_length;
        t = original.t;
        t[row-1].f_string[col-1] = newele;
    }
    tbl(tbl original, int row, string type) {
        col_length = original.col_length;
        row_length = original.row_length;
        t = original.t;
        int newtype;
        if (type.compare("int") == 0) {
            newtype = 0;
        } else if (type.compare("doulbe") == 0) {
            newtype = 1;
        } else if (type.compare("string") == 0) {
            newtype = 2;
        }
        if (t[row-1].type != newtype) {
            if (newtype == 2) {
                if (t[row-1].type == 0) {
                    t[row-1].type = 2;
                    for (int i=0; i<col_length; i++) {
                        stringstream strStream;
                        strStream << t[row-1].f_int[i];
                        string s = strStream.str();
                        t[row-1].f_string.push_back(s);
                    }
                }
                if (t[row-1].type == 1) {
                    t[row-1].type = 2;
                    for (int i=0; i<col_length; i++) {
                        stringstream strStream;
                        strStream << t[row-1].f_double[i];
                        string s = strStream.str();
                        t[row-1].f_string.push_back(s);
                        cout << t[row-1].f_string[i] << endl;
                    }
                }
            }
            else if (t[row-1].type == 2) {
                if (newtype == 0) {
                    t[row-1].type = 0;
                    for (int i=0; i<col_length; i++) {
                        char temp[10];
                        strcpy(temp, t[row-1].f_string[i].c_str());
                        t[row-1].f_int[i] = atoi(temp);
                    }
                }
                if (newtype == 1) {
                    t[row-1].type = 1;
                    for (int i=0; i<row_length; i++) {
                        char temp[10];
                        strcpy(temp, t[row-1].f_string[i].c_str());
                        t[row-1].f_int[i] = atof(temp);
                    }
                }     
            } else if (newtype == 1) {
                t[row-1].type = 1;
                for (int i=0; i<row_length; i++) {
                    t[row-1].f_double[i] = (double) t[row-1].f_int[i];
                }
            } else if (newtype == 0) {
                t[row-1].type = 0;
                for (int i=0; i<row_length; i++) {
                    t[row-1].f_int[i] = (int) t[row-1].f_double[i];
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

fld access(tbl source, string fname) {
    for (int i=0; i<source.row_length; i++) {
        if (source.t[i].name.compare(fname) == 0) {
            if (source.t[i].type == 0) {
                vector<int> array;
                for (int j=0; j<source.col_length; j++) {
                    array.push_back(source.t[i].f_int[j]);
                }
                fld target = fld(&array[0], fname, source.col_length);
                return target;
            } else if (source.t[i].type == 1) {
                vector<double> array;
                for (int j=0; j<source.col_length; j++) {
                    array.push_back(source.t[i].f_double[j]);
                }
                fld target = fld(&array[0], fname, source.col_length);
                return target;
            } else if (source.t[i].type == 2) {
                vector<string> array;
                for (int j=0; j<source.col_length; j++) {
                    array.push_back(source.t[i].f_string[j]);
                }
                fld target = fld(&array[0], fname, source.col_length);
                return target;
            }
        }
    }
    cout << "Not find the access."<<endl;
    vector<int> null;
    fld empty = fld(&null[0], fname, 0);
    return empty;
}

rec access(tbl source, int num) {
    vector<tuple> tuples;
    if (num > source.col_length-1) {
        cout << "Exceed the scope."<<endl;
        rec target = rec(&tuples[0], 0);
        return target;
    } else {
        for (int i=0; i<source.row_length; i++) {
            if (source.t[i].type == 0) {
                tuple temp = tuple(source.t[i].f_int[num], source.t[i].name);
                tuples.push_back(temp);
            } else if (source.t[i].type == 1) {
                tuple temp = tuple(source.t[i].f_double[num], source.t[i].name);
                tuples.push_back(temp);
            } else if (source.t[i].type == 2) {
                tuple temp = tuple(source.t[i].f_string[num], source.t[i].name);
                tuples.push_back(temp);
            }
        }
        rec target = rec(&tuples[0], source.row_length);
        return target;
    }
}

tbl access(tbl source, int start, int end) {
    if (start > source.col_length-1) {
        cout << "Exceed the scope."<<endl;
        vector<rec> null;
        tbl target = tbl(&null[0], 0, source.row_length);
        return target;
    }
    vector<rec> store; 
    for (int num = start; num < end; num++) {
        vector<tuple> tuples;
        for (int i=0; i<source.row_length; i++) {
            if (source.t[i].type == 0) {
                tuple temp = tuple(source.t[i].f_int[num], source.t[i].name);
                tuples.push_back(temp);
            } else if (source.t[i].type == 1) {
                tuple temp = tuple(source.t[i].f_double[num], source.t[i].name);
                tuples.push_back(temp);
            } else if (source.t[i].type == 2) {
                tuple temp = tuple(source.t[i].f_string[num], source.t[i].name);
                tuples.push_back(temp);
            }
        }
        rec temp = rec(&tuples[0], source.row_length);
        store.push_back(temp);
        if (end == source.col_length - 1) {
            break;
        }
    }
    tbl target = tbl(&store[0], store.size(), source.row_length);
    return target;
}

tbl access(tbl source, string *names, int length) {
    vector<fld> store;
    for (int k=0; k<length; k++) {
        string fname = names[k];
        for (int i=0; i<source.row_length; i++) {
            if (source.t[i].name.compare(fname) == 0) {
                if (source.t[i].type == 0) {
                    vector<int> array;
                    for (int j=0; j<source.col_length; j++) {
                        array.push_back(source.t[i].f_int[j]);
                    }
                    fld temp = fld(&array[0], fname, source.col_length);
                    store.push_back(temp);
                } else if (source.t[i].type == 1) {
                    vector<double> array;
                    for (int j=0; j<source.col_length; j++) {
                        array.push_back(source.t[i].f_double[j]);
                    }
                    fld temp = fld(&array[0], fname, source.col_length);
                    store.push_back(temp);
                } else if (source.t[i].type == 2) {
                    vector<string> array;
                    for (int j=0; j<source.col_length; j++) {
                        array.push_back(source.t[i].f_string[j]);
                    }
                    fld temp = fld(&array[0], fname, source.col_length);
                    store.push_back(temp);
                }
            }
        }
    }
    tbl target = tbl(&store[0], source.col_length, store.size());
    return target;
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

/*int main(int argc, char const *argv[]) {
    for (int c = 0; c < 0; c++) {
        cout << "Hello World" << endl;
    }
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
    tuple j[] = {tuple ("James", "name"), tuple (20, "age")};
    rec k = rec (j, getArrayLen(j));
    tuple u[] = {tuple (21, "age"), tuple ("Min", "name")};
    rec v = rec (u, getArrayLen(u));
    cout << k;
    fld i[] = {b,h};
    rec l[] = {f,k,v};
    tbl t = tbl(i, i[0].length, getArrayLen(i));
    // sample definition of tbl (kind 1)
    cout << t;
    fld tt = access(t, "word");
    cout << tt;
    rec ttt = access(t, 2);
    cout << ttt;
    tbl sss = access(t, 1, 3);
    cout << sss;
    string ta[] = {"word", "age", "value"};
    tbl stt = access(t, ta, getArrayLen(ta));
    cout << stt;
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
    // sample definition of changing
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
} */
