#include "dave.hpp"
#include <fstream>
using namespace std;

tbl tbl_read(string filename) {
    char buffer[256];
    ifstream in(filename);
    bool flag = false;
    vector< rec > storage;
    vector< string > name;
    vector< string > element;
    vector< tuple > tuples;

    if (! in.is_open()) {
    	cout << "File opening errorness" << endl;
    	exit(1);
    }
    while (! in.eof()) {
        if (!flag) {
            in.getline(buffer, 256, '\n');
            string whole = string(buffer);
            int i = 0;
            int j = 0;
            while (buffer[i+j] != ';') {
                // note here, every line need to be end with " ;"
                if (buffer[i+j] == ' ') {
                    string temp = whole.substr(i, j);
                    cout << temp << endl;
                    name.push_back(temp);
                    i = i+j+1;
                    j = 0;
                    continue;
                }
                j++;
            }
            flag = true;
            continue; 
        }
        element.clear();
        tuples.clear();
        in.getline(buffer, 256, '\n');
        string whole = string(buffer);
        int i = 0;
        int j = 0;
        while (buffer[i+j] != ';') {
            if (buffer[i+j] == ' ') {
                string temp = whole.substr(i, j);
                cout << temp << endl;
                element.push_back(temp);
                i = i+j+1;
                j = 0;
                continue;
            }
            j++;
        }
        for (int k=0; k<name.size(); k++) {
            tuple newtup = tuple(element[k], name[k]);
            tuples.push_back(newtup);
        }
        rec newrec = rec(&tuples[0], tuples.size());
        storage.push_back(newrec);
    }
    in.close();
    tbl readtbl = tbl(&storage[0], storage[0].length, storage.size());
    return readtbl;
}

void tbl_write(tbl &in, string filename) {
    ofstream out;
    out.open(filename);
    for (int i=0; i<in.row_length; i++) {
        if (i<in.row_length-1) {
            out << in.t[i].name << "\t";
        } else {
            out << in.t[i].name << endl;
        }
    }
    for (int i=0; i<in.col_length; i++) {
        out << "[";
        for (int j=0; j<in.row_length; j++) {
            if ( in.t[j].type == 0 ) {
                out << in.t[j].f_int[i];
                if (j < in.row_length-1) {
                    out << ",\t";
                }
            } else if ( in.t[j].type == 1 ) {
                out << in.t[j].f_double[i];
                if (j < in.row_length-1) {
                    out << ",\t";
                }
            } else if ( in.t[j].type == 2 ) {
                out << in.t[j].f_string[i];
                if (j < in.row_length-1) {
                    out << ",\t";
                }
            }
        }
        out << "]" << endl;
    }
    out.close();
}

/*
int main(int argc, char const *argv[]) {
	tbl test = tbl_read(argv[1]);
	cout << test;
    tbl_write(test, argv[2]);
	return 0;
}*/
