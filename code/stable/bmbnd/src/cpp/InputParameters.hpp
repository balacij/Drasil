/** \file InputParameters.hpp
    \author Jason Balaci
    \brief Provides the function for reading inputs
*/
#ifndef InputParameters_h
#define InputParameters_h

#include <string>

using std::ifstream;
using std::string;

/** \brief Reads input from a file with the given file name
    \param filename name of the input file
    \param a_0 coefficient of w_B's term of power 0 (N/m)
    \param a_1 coefficient of w_B's term of power 1 (N/m^2)
    \param a_2 coefficient of w_B's term of power 2 (N/m^3)
    \param a_3 coefficient of w_B's term of power 3 (N/m^4)
    \param L_B length of the beam (m)
    \param E_B modulus of elasticity of the beam (Pa)
    \param I_B moment of second area of a cross-section of the beam (m)
*/
void get_input(string filename, double &a_0, double &a_1, double &a_2, double &a_3, double &L_B, double &E_B, double &I_B);

#endif
