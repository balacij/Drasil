package BmBnd;

/** \file InputParameters.java
    \author Jason Balaci
    \brief Provides the function for reading inputs
*/
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class InputParameters {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \return array containing the following values:
        \return coefficient of w_B's term of power 0 (N/m)
        \return coefficient of w_B's term of power 1 (N/m^2)
        \return coefficient of w_B's term of power 2 (N/m^3)
        \return coefficient of w_B's term of power 3 (N/m^4)
        \return length of the beam (m)
        \return modulus of elasticity of the beam (Pa)
        \return moment of second area of a cross-section of the beam (m)
    */
    public static Object[] get_input(String filename) throws FileNotFoundException {
        double a_0;
        double a_1;
        double a_2;
        double a_3;
        double L_B;
        double E_B;
        double I_B;
        
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        a_0 = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        a_1 = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        a_2 = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        a_3 = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        L_B = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        E_B = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        I_B = Double.parseDouble(infile.nextLine());
        infile.close();
        
        Object[] outputs = new Object[7];
        outputs[0] = a_0;
        outputs[1] = a_1;
        outputs[2] = a_2;
        outputs[3] = a_3;
        outputs[4] = L_B;
        outputs[5] = E_B;
        outputs[6] = I_B;
        return outputs;
    }
}
