#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

using std::ofstream;
using std::string;
using std::vector;

void write_output(vector<double> &y_B) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "y_B = ";
    outputfile << "[";
    for (int list_i1 = 0; list_i1 < (int)(y_B.size()) - 1; list_i1++) {
        outputfile << y_B.at(list_i1);
        outputfile << ", ";
    }
    if ((int)(y_B.size()) > 0) {
        outputfile << y_B.at((int)(y_B.size()) - 1);
    }
    outputfile << "]" << std::endl;
    outputfile.close();
}
