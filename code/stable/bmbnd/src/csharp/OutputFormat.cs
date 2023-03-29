/** \file OutputFormat.cs
    \author Jason Balaci
    \brief Provides the function for writing outputs
*/
using System;
using System.Collections.Generic;
using System.IO;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param y_B deflection at a particular point along the beam (m)
    */
    public static void write_output(List<double> y_B) {
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("y_B = ");
        outputfile.Write("[");
        for (int list_i1 = 0; list_i1 < y_B.Count - 1; list_i1++) {
            outputfile.Write(y_B[list_i1]);
            outputfile.Write(", ");
        }
        if (y_B.Count > 0) {
            outputfile.Write(y_B[y_B.Count - 1]);
        }
        outputfile.WriteLine("]");
        outputfile.Close();
    }
}
