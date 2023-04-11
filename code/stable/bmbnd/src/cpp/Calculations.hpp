/** \file Calculations.hpp
    \author Jason Balaci
    \brief Provides functions for calculating the outputs
*/
#ifndef Calculations_h
#define Calculations_h

#include <vector>

using std::vector;

/** \brief Calculates deflection at a particular point along the beam (m)
    \param E_B modulus of elasticity of the beam (Pa)
    \return deflection at a particular point along the beam (m)
*/
vector<double> func_y_B(double E_B);

#endif