/** \file OutputFormat.hpp
    \author Jason Balaci
    \brief Provides the function for writing outputs
*/
#ifndef OutputFormat_h
#define OutputFormat_h

#include <string>
#include <vector>

using std::ofstream;
using std::string;
using std::vector;

/** \brief Writes the output values to output.txt
    \param y_B deflection at a particular point along the beam (m)
*/
void write_output(vector<double> &y_B);

#endif
