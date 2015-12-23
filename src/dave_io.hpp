#include "dave.hpp"
#include <fstream>
using namespace std;

tbl load(string filename) {
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
                if (buffer[i+j] == '\t') {
                    string temp = whole.substr(i, j);
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
            if (buffer[i+j] == '\t') {
                string temp = whole.substr(i, j);
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
    tbl readtbl = tbl(&storage[0], storage.size(), storage[0].length);
    return readtbl;
}

void save(tbl &in, string filename) {
    ofstream out;
    out.open(filename);
    for (int i=0; i<in.row_length; i++) {
        out << in.t[i].name << "\t";
    }
    out << ";" << endl;
    for (int i=0; i<in.col_length; i++) {
        for (int j=0; j<in.row_length; j++) {
            if ( in.t[j].type == 0 ) {
                out << in.t[j].f_int[i];
            } else if ( in.t[j].type == 1 ) {
                out << in.t[j].f_double[i];
            } else if ( in.t[j].type == 2 ) {
                out << in.t[j].f_string[i];
            } else if ( in.t[j].type == 3 ) {
                if (in.t[j].f_bool[i] == 0) {
                    out << "false";
                } else {
                    out << "true";
                }
            }
            out << "\t";
        }
        if (i != in.col_length-1) {
            out << ";" << endl;
        } else {
            out << ";";
        }
    }
    out.close();
}

/*int main(int argc, char const *argv[]) {
	tbl test = tbl_read(argv[1]);
	cout << test;
    tbl_write(test, argv[2]);
    test = tbl_read(argv[2]);
    cout << test;
	return 0;
}*/