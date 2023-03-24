/** \file InputParameters.cs
    \author Jason Balaci
    \brief Provides the function for reading inputs
*/
using System;
using System.IO;

public class InputParameters {
    
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
    public static void get_input(string filename, out double a_0, out double a_1, out double a_2, out double a_3, out double L_B, out double E_B, out double I_B) {
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        a_0 = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        a_1 = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        a_2 = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        a_3 = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        L_B = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        E_B = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        I_B = Double.Parse(infile.ReadLine());
        infile.Close();
    }
}
