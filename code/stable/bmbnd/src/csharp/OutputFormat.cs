/** \file OutputFormat.cs
    \author Jason Balaci
    \brief Provides the function for writing outputs
*/
using System;
using System.IO;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param y_B deflection at a particular point along the beam (m)
    */
    public static void write_output(Func<double, float, double> y_B) {
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("y_B = ");
        outputfile.WriteLine(y_B);
        outputfile.Close();
    }
}
