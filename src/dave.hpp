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
    bool content_bool;
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
        if (str.compare("true") == 0) {
            type = 3;
            content_bool = true;
            name = str;
        } else if (str.compare("false") == 0) {
            type = 3;
            content_bool = false;
            name = str;
        } else {
            type = 2;
            content_string = initial;
            name = str;
        }
    }
    /*tuple(bool initial,string str) {
        type = 3;
        content_bool = initial;
        name = str;
    }*/
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
    vector<bool> f_bool; 
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
    fld(bool *array, string str, int len) {
        type = 3;
        length = len;
        name = str;
        for (int i=0; i<length; i++)
            f_bool.push_back(array[i]);
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
            } else if (array[0].r[i].type == 3) {
                bool data[col_len];
                for(int j=0; j<col_len; j++) {
                    for (int k=0; k<row_len; k++) {
                        if (array[j].r[k].name.compare(name) == 0) {
                            data[j] = array[j].r[k].content_bool;
                        }
                    }
                }
                fld temp = fld (data, name, col_len);
                t.push_back(temp);
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
        t[row].f_int[col] = newele;
    }
    tbl(tbl original, int col, int row, double newele) {
        if (col > original.col_length || row > original.row_length) {
            cout << "reset failed!" << endl;
            exit(1);
        }
        col_length = original.col_length;
        row_length = original.row_length;
        t = original.t;
        t[row].f_double[col] = newele;
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
    /* tbl(tbl original, int col, int row, bool newele) {
        if (col > original.col_length || row > original.row_length) {
            cout << "reset failed!" << endl;
            exit(1);
        }
        col_length = original.col_length;
        row_length = original.row_length;
        t = original.t;
        t[row-1].f_bool[col-1] = newele;
    }*/
}; 

tbl append(tbl original, fld newfld) {
    for (int j=0; j<original.row_length; j++) {
        if (newfld.name.compare(original.t[j].name) == 0) {
            cout << "the name has already existed" << endl;
            return original;
        }
    }
    original.row_length ++;
    original.t.push_back(newfld);
    return original;
}

tbl append(tbl original, rec newrec) {
    original.col_length ++;
    for (int i=0; i<newrec.length; i++) {
        for (int j=0; j<original.row_length; j++) {
            if (newrec.r[i].name.compare(original.t[j].name) == 0) {
                if (newrec.r[i].type == 0) {
                    original.t[j].f_int.push_back(newrec.r[i].content_int);
                } else if (newrec.r[i].type == 1) {
                    original.t[j].f_double.push_back(newrec.r[i].content_double);
                } else if (newrec.r[i].type == 2) {
                    original.t[j].f_string.push_back(newrec.r[i].content_string);
                } else if (newrec.r[i].type == 3) {
                    original.t[j].f_bool.push_back(newrec.r[i].content_bool);
                } 
                break;
            }
        }
    }
    return original;
}

tbl append(tbl source1, tbl source2, bool ishorizonal) {
    if (ishorizonal) {
        if (source1.col_length != source2.col_length) {
            cout << "combine failed!" << endl;
            exit(1);
        }
        source1.row_length = source1.row_length + source2.row_length;
        for (int i=0; i<source2.row_length; i++) {
            source1.t.push_back(source2.t[i]);
        }
    } else {
        if (source1.row_length != source2.row_length) {
            cout << "combine failed!" << endl;
            exit(1);
        }
        source1.col_length = source1.col_length + source2.col_length;
        for (int i=0; i<source2.row_length; i++) {
            for (int j=0; j<source1.row_length; j++) {
                if (source2.t[i].name.compare(source1.t[j].name) == 0) {
                    for (int k=0; k< source2.t[i].length; k++) {
                        if (source2.t[i].type == 0) {
                            source1.t[j].f_int.push_back(source2.t[i].f_int[k]);
                        } else if (source2.t[i].type == 1) {
                            source1.t[j].f_double.push_back(source2.t[i].f_double[k]);
                        } else if (source2.t[i].type == 2) {
                            source1.t[j].f_string.push_back(source2.t[i].f_string[k]); 
                        } else if (source2.t[i].type == 3) {
                            source1.t[j].f_bool.push_back(source2.t[i].f_bool[k]); 
                        }
                    }
                    break;
                }
            }
        }
    }
    return source1;
}

tbl modify(tbl original, int col, int row, int newele) {
    if (original.t[row].type != 0) {
        cout << "type is wrong" << endl;
        exit(1);
    }
    if (col > original.col_length || row > original.row_length) {
        cout << "reset failed!" << endl;
        exit(1);
    }
    original.t[row].f_int[col] = newele;
    return original;
}
tbl modify(tbl original, int col, int row, double newele) {
    if (original.t[row].type != 1) {
        cout << "type is wrong" << endl;
        exit(1);
    }
    if (col > original.col_length || row > original.row_length) {
        cout << "reset failed!" << endl;
        exit(1);
    }
    original.t[row].f_double[col] = newele;
    return original;
}
tbl modify(tbl original, int col, int row, string newele) {
    if (col > original.col_length || row > original.row_length) {
        cout << "reset failed!" << endl;
        exit(1);
    }
    if (original.t[row].type == 2) {
        if (newele.compare("false") != 0 && newele.compare("true") != 0) {
            original.t[row].f_string[col] = newele;
        }
    } else if (original.t[row].type == 3) {
        if (newele.compare("false") == 0) {
            original.t[row].f_bool[col] = 0;
        } else if (newele.compare("true") == 0) {
            original.t[row].f_bool[col] = 1;
        }
    } else {
        cout << "type is wrong" << endl;
        exit(1);
    }
    return original;
}

tbl converse(tbl original, int row, string type) {
    int newtype;
    if (type.compare("int") == 0) {
        newtype = 0;
    } else if (type.compare("doulbe") == 0) {
        newtype = 1;
    } else if (type.compare("string") == 0) {
        newtype = 2;
    }
    if (original.t[row-1].type != newtype) {
        if (newtype == 2) {
            if (original.t[row-1].type == 0) {
                original.t[row-1].type = 2;
                for (int i=0; i<original.col_length; i++) {
                    stringstream strStream;
                    strStream << original.t[row-1].f_int[i];
                    string s = strStream.str();
                    original.t[row-1].f_string.push_back(s);
                }
            }
            if (original.t[row-1].type == 1) {
                original.t[row-1].type = 2;
                for (int i=0; i<original.col_length; i++) {
                    stringstream strStream;
                    strStream << original.t[row-1].f_double[i];
                    string s = strStream.str();
                    original.t[row-1].f_string.push_back(s);
                }
            }
        }
        else if (original.t[row-1].type == 2) {
            if (newtype == 0) {
                original.t[row-1].type = 0;
                for (int i=0; i<original.col_length; i++) {
                    char temp[10];
                    strcpy(temp, original.t[row-1].f_string[i].c_str());
                    original.t[row-1].f_int[i] = atoi(temp);
                }
            }
            if (newtype == 1) {
                original.t[row-1].type = 1;
                for (int i=0; i<original.row_length; i++) {
                    char temp[10];
                    strcpy(temp, original.t[row-1].f_string[i].c_str());
                    original.t[row-1].f_int[i] = atof(temp);
                }
            }     
        } else if (newtype == 1) {
            original.t[row-1].type = 1;
            for (int i=0; i<original.row_length; i++) {
                original.t[row-1].f_double[i] = (double) original.t[row-1].f_int[i];
            }
        } else if (newtype == 0) {
            original.t[row-1].type = 0;
            for (int i=0; i<original.row_length; i++) {
                original.t[row-1].f_int[i] = (int) original.t[row-1].f_double[i];
            }
        }
    }
    return original;
}


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

tbl mult(tbl source1, tbl source2) {
    if (source1.col_length != source2.col_length ||
        source1.row_length != source2.row_length) {
        return source1;
    }
    for (int i=0; i<source1.col_length; i++) {
        for (int j=0; j<source1.row_length; j++) {
            if (source1.t[j].type == 0 && source2.t[j].type == 0) {
                source1.t[j].f_int[i] *= source2.t[j].f_int[i];
            }
            else if (source1.t[j].type == 1 && source2.t[j].type == 1) {
                source1.t[j].f_double[i] *= source2.t[j].f_double[i];
            }
        }
    }
    return source1;
}

tbl div(tbl source1, tbl source2) {
    if (source1.col_length != source2.col_length ||
        source1.row_length != source2.row_length) {
        return source1;
    }
    for (int i=0; i<source1.col_length; i++) {
        for (int j=0; j<source1.row_length; j++) {
            if (source1.t[j].type == 0 && source2.t[j].type == 0) {
                source1.t[j].f_int[i] /= source2.t[j].f_int[i];
            }
            else if (source1.t[j].type == 1 && source2.t[j].type == 1) {
                source1.t[j].f_double[i] /= source2.t[j].f_double[i];
            }
        }
    }
    return source1;
}

/*int access_int(rec source, string name) {
    int result;
    for (int i=0; i<source.length; i++) {
        if (source.r[i].name.compare(name) == 0) {
            result = source.r[i].content_int;
            return result;
        }
    }
    cout << "Not find the access."<<endl;
    return 0;
}

double access_double(rec source, string name) {
    double result;
    for (int i=0; i<source.length; i++) {
        if (source.r[i].name.compare(name) == 0) {
            result = source.r[i].content_double;
            return result;
        }
    }
    cout << "Not find the access."<<endl;
    return 0.0;
}

string access_string(rec source, string name) {
    string result;
    for (int i=0; i<source.length; i++) {
        if (source.r[i].name.compare(name) == 0) {
            result = source.r[i].content_string;
            return result;
        }
    }
    cout << "Not find the access."<<endl;
    return "";
}

bool access_bool(rec source, string name) {
    bool result;
    for (int i=0; i<source.length; i++) {
        if (source.r[i].name.compare(name) == 0) {
            result = source.r[i].content_bool;
            return result;
        }
    }
    cout << "Not find the access."<<endl;
    return 0;
}*/

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
            } else if (source.t[i].type == 3) {
                bool array[source.col_length];
                for (int j=0; j<source.col_length; j++) {
                    array[j] = source.t[i].f_bool[j];
                }
                fld target = fld(array, fname, source.col_length);
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
            } else if (source.t[i].type == 3) {
                tuple temp = tuple(source.t[i].f_bool[num], source.t[i].name);
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
            } else if (source.t[i].type == 3) {
                tuple temp = tuple(source.t[i].f_bool[num], source.t[i].name);
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
                } else if (source.t[i].type == 3) {
                    bool array[source.col_length];
                    for (int j=0; j<source.col_length; j++) {
                        array[j] = source.t[i].f_bool[j];
                    }
                    fld temp = fld(array, fname, source.col_length);
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
    } else if (in.type == 3) {
        if (in.content_bool == 0) {
            sys << "false";
        } else {
            sys << "true";
        }
    }
    sys << endl;
    return sys;
}

ostream & operator << (ostream &sys, const rec &in) {
	for (int i=0; i<in.length; i++) {
		sys << in.r[i].name << "\t";
	} 
    sys << ";" << endl;
	for (int i=0; i<in.length; i++) {
        if (in.r[i].type == 0) {
            sys << in.r[i].content_int;
        } else if (in.r[i].type == 1) {
            sys << in.r[i].content_double;
        } else if (in.r[i].type == 2) {
            sys << in.r[i].content_string;
        } else if (in.r[i].type == 3) {
            if (in.r[i].content_bool == 0) {
                sys << "false";
            } else {
                sys << "true";
            }
        }
		sys << "\t";
	}
	sys << ";" << endl;
	return sys;
}

ostream & operator << (ostream &sys, const fld &in) {
	sys << in.name << "\t;" << endl;
    if ( in.type == 0 ) {
	    for (int i=0; i<in.length; i++)
		    sys << in.f_int[i] << "\t;" << endl;
    } else if ( in.type == 1 ) {
        for (int i=0; i<in.length; i++)
            sys << in.f_double[i] << "\t;" << endl;
    } else if ( in.type == 2 ) {
        for (int i=0; i<in.length; i++)
            sys << in.f_string[i] << "\t;" << endl; 
    } else if ( in.type == 3 ) {
        for (int i=0; i<in.length; i++) {
            if (in.f_bool[i] == 0) {
                sys << "false" << "\t;" << endl;
            } else {
                sys << "true" << "\t;" << endl;
            }
        }
    }
	return sys;
}

ostream & operator << (ostream &sys, const tbl &in) {
    for (int i=0; i<in.row_length; i++) {
		sys << in.t[i].name << "\t";
    }
    sys << ";" << endl;
    for (int i=0; i<in.col_length; i++) {
        for (int j=0; j<in.row_length; j++) {
            if ( in.t[j].type == 0 ) {
                sys << in.t[j].f_int[i];
            } else if ( in.t[j].type == 1 ) {
                sys << in.t[j].f_double[i];
            } else if ( in.t[j].type == 2 ) {
                sys << in.t[j].f_string[i];
            } else if ( in.t[j].type == 3 ) {
                if (in.t[j].f_bool[i] == 0) {
                    sys << "false";
                } else {
                    sys << "true";
                }
            }
            sys << "\t";
        }
    	sys << ";" << endl;
    }
    return sys;
}

/*int main(int argc, char const *argv[]) {
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
    tbl n = append(s, b);
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
    tbl z = append(s, w);
    // sample definition of binding (kind 2)
    cout << "result of binding2 is:" << endl;
    cout << z;
    tbl st = append(s, t, true);
    // sample definition of binding (kind 3)
    cout << "result of binding3 is:" << endl;
    cout << st;
    tbl sx = append(s, x, false);
    // sample definition of binding (kind 4)
    cout << "result of binding4 is:" << endl;
    cout << sx;
    tbl ss = modify(sx, 0, 1, "Cesc");
    // sample definition of changing
    cout << "result of changing is:" << endl;
    cout << ss;
    tbl sx1 = converse(sx, 1, "string");
    cout << "result of conversion is:" << endl;
    cout << sx1;
    tbl sx2 = converse(sx1, 1, "int");
    cout << "result of conversion is:" << endl;
    cout << sx2;
    // sample of data type conversion
    return 0; 
}*/
