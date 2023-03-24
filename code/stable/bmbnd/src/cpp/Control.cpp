/** \file Control.cpp
    \author Jason Balaci
    \brief Controls the flow of the program
*/
#include <string>

#include "Calculations.hpp"
#include "InputParameters.hpp"
#include "OutputFormat.hpp"

using std::string;

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
*/
int main(int argc, const char *argv[]) {
    string filename = argv[1];
    double a_0;
    double a_1;
    double a_2;
    double a_3;
    double L_B;
    double E_B;
    double I_B;
    get_input(filename, a_0, a_1, a_2, a_3, L_B, E_B, I_B);
    y_B = func_y_B(E_B);
    write_output(y_B);
    
    return 0;
}
