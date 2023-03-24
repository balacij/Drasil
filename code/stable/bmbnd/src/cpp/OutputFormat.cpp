#include "OutputFormat.hpp"

#include <fstream>
#include <iostream>
#include <string>

using std::ofstream;
using std::string;

void write_output(y_B) {
    ofstream outputfile;
    outputfile.open("output.txt", std::fstream::out);
    outputfile << "y_B = ";
    outputfile << y_B << std::endl;
    outputfile.close();
}
