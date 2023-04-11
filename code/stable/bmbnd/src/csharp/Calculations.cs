/** \file Calculations.cs
    \author Jason Balaci
    \brief Provides functions for calculating the outputs
*/
using System.Collections.Generic;

public class Calculations {
    
    /** \brief Calculates deflection at a particular point along the beam (m)
        \param E_B modulus of elasticity of the beam (Pa)
        \return deflection at a particular point along the beam (m)
    */
    public static List<double> func_y_B(double E_B) {
        return E_B;
    }
}