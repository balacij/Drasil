package BmBnd;

/** \file OutputFormat.java
    \author Jason Balaci
    \brief Provides the function for writing outputs
*/
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param y_B deflection at a particular point along the beam (m)
    */
    public static void write_output(ArrayList<Double> y_B) throws IOException {
        PrintWriter outputfile;
        outputfile = new PrintWriter(new FileWriter(new File("output.txt"), false));
        outputfile.print("y_B = ");
        outputfile.println(y_B);
        outputfile.close();
    }
}
