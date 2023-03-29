/** \file Control.cs
    \author Jason Balaci
    \brief Controls the flow of the program
*/
using System.Collections.Generic;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void Main(string[] args) {
        string filename = args[0];
        double a_0;
        double a_1;
        double a_2;
        double a_3;
        double L_B;
        double E_B;
        double I_B;
        InputParameters.get_input(filename, out a_0, out a_1, out a_2, out a_3, out L_B, out E_B, out I_B);
        List<double> y_B = Calculations.func_y_B(E_B);
        OutputFormat.write_output(y_B);
    }
}
