package BmBnd;

/** \file Control.java
    \author Jason Balaci
    \brief Controls the flow of the program
*/
import java.io.FileNotFoundException;
import java.io.IOException;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void main(String[] args) throws FileNotFoundException, IOException {
        String filename = args[0];
        double a_0;
        double a_1;
        double a_2;
        double a_3;
        double L_B;
        double E_B;
        double I_B;
        Object[] outputs = InputParameters.get_input(filename);
        a_0 = (double)(outputs[0]);
        a_1 = (double)(outputs[1]);
        a_2 = (double)(outputs[2]);
        a_3 = (double)(outputs[3]);
        L_B = (double)(outputs[4]);
        E_B = (double)(outputs[5]);
        I_B = (double)(outputs[6]);
        y_B = Calculations.func_y_B(E_B);
        OutputFormat.write_output(y_B);
    }
}
