package GlassBR;

/** \file Calculations.java
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides functions for calculating the outputs
*/
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class Calculations {
    
    /** \brief Calculates glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
        \param inParams structure holding the input values
        \return glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
    */
    public static int func_GTF(InputParameters inParams) throws Exception, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_GTF called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        if (inParams.g.equals("AN")) {
            return 1;
        }
        else if (inParams.g.equals("FT")) {
            return 4;
        }
        else if (inParams.g.equals("HS")) {
            return 2;
        }
        else {
            throw new Exception("Undefined case encountered in function func_GTF");
        }
    }
    
    /** \brief Calculates aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
        \param inParams structure holding the input values
        \return aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
    */
    public static double func_AR(InputParameters inParams) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_AR called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        return inParams.a / inParams.b;
    }
    
    /** \brief Calculates stress distribution factor (Function) based on Pbtol
        \param inParams structure holding the input values
        \return stress distribution factor (Function) based on Pbtol
    */
    public static double func_J_tol(InputParameters inParams) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_J_tol called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        return Math.log(Math.log(1.0 / (1.0 - inParams.P_btol)) * (Math.pow(inParams.a * inParams.b, 7.0 - 1.0) / (2.86e-53 * Math.pow(7.17e10 * Math.pow(inParams.h, 2.0), 7.0) * inParams.LDF)));
    }
    
    /** \brief Calculates applied load (demand): 3 second duration equivalent pressure (Pa)
        \param inParams structure holding the input values
        \return applied load (demand): 3 second duration equivalent pressure (Pa)
    */
    public static double func_q(InputParameters inParams) throws Exception, FileNotFoundException, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_q called with inputs: {");
        outfile.print("  inParams = ");
        outfile.println("Instance of InputParameters object");
        outfile.println("  }");
        outfile.close();
        
        return Interpolation.interpY("TSD.txt", inParams.SD, inParams.w_TNT);
    }
    
    /** \brief Calculates dimensionless load
        \param inParams structure holding the input values
        \param q applied load (demand): 3 second duration equivalent pressure (Pa)
        \param GTF glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
        \return dimensionless load
    */
    public static double func_q_hat(InputParameters inParams, double q, int GTF) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_q_hat called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  q = ");
        outfile.print(q);
        outfile.println(", ");
        outfile.print("  GTF = ");
        outfile.println(GTF);
        outfile.println("  }");
        outfile.close();
        
        return q * Math.pow(inParams.a * inParams.b, 2.0) / (7.17e10 * Math.pow(inParams.h, 4.0) * GTF);
    }
    
    /** \brief Calculates tolerable load
        \param AR aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
        \param J_tol stress distribution factor (Function) based on Pbtol
        \return tolerable load
    */
    public static double func_q_hat_tol(double AR, double J_tol) throws Exception, FileNotFoundException, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_q_hat_tol called with inputs: {");
        outfile.print("  AR = ");
        outfile.print(AR);
        outfile.println(", ");
        outfile.print("  J_tol = ");
        outfile.println(J_tol);
        outfile.println("  }");
        outfile.close();
        
        return Interpolation.interpY("SDF.txt", AR, J_tol);
    }
    
    /** \brief Calculates stress distribution factor (Function)
        \param AR aspect ratio: the ratio of the long dimension of the glass to the short dimension of the glass. For glass supported on four sides, the aspect ratio is always equal to or greater than 1.0. For glass supported on three sides, the ratio of the length of one of the supported edges perpendicular to the free edge, to the length of the free edge, is equal to or greater than 0.5
        \param q_hat dimensionless load
        \return stress distribution factor (Function)
    */
    public static double func_J(double AR, double q_hat) throws Exception, FileNotFoundException, IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_J called with inputs: {");
        outfile.print("  AR = ");
        outfile.print(AR);
        outfile.println(", ");
        outfile.print("  q_hat = ");
        outfile.println(q_hat);
        outfile.println("  }");
        outfile.close();
        
        return Interpolation.interpZ("SDF.txt", AR, q_hat);
    }
    
    /** \brief Calculates non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
        \param inParams structure holding the input values
        \param q_hat_tol tolerable load
        \return non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
    */
    public static double func_NFL(InputParameters inParams, double q_hat_tol) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_NFL called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  q_hat_tol = ");
        outfile.println(q_hat_tol);
        outfile.println("  }");
        outfile.close();
        
        return q_hat_tol * 7.17e10 * Math.pow(inParams.h, 4.0) / Math.pow(inParams.a * inParams.b, 2.0);
    }
    
    /** \brief Calculates risk of failure
        \param inParams structure holding the input values
        \param J stress distribution factor (Function)
        \return risk of failure
    */
    public static double func_B(InputParameters inParams, double J) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_B called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  J = ");
        outfile.println(J);
        outfile.println("  }");
        outfile.close();
        
        return 2.86e-53 / Math.pow(inParams.a * inParams.b, 7.0 - 1.0) * Math.pow(7.17e10 * Math.pow(inParams.h, 2.0), 7.0) * inParams.LDF * Math.exp(J);
    }
    
    /** \brief Calculates load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
        \param NFL non-factored load: three second duration uniform load associated with a probability of breakage less than or equal to 8 lites per 1000 for monolithic AN glass (Pa)
        \param GTF glass type factor: a multiplying factor for adjusting the LR of different glass type, that is, AN, FT, or HS, in monolithic glass, LG (Laminated Glass), or IG (Insulating Glass) constructions
        \return load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
    */
    public static double func_LR(double NFL, int GTF) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_LR called with inputs: {");
        outfile.print("  NFL = ");
        outfile.print(NFL);
        outfile.println(", ");
        outfile.print("  GTF = ");
        outfile.println(GTF);
        outfile.println("  }");
        outfile.close();
        
        return NFL * GTF * 1.0;
    }
    
    /** \brief Calculates probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
        \param B risk of failure
        \return probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
    */
    public static double func_P_b(double B) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_P_b called with inputs: {");
        outfile.print("  B = ");
        outfile.println(B);
        outfile.println("  }");
        outfile.close();
        
        return 1.0 - Math.exp(-B);
    }
    
    /** \brief Calculates 3 second load equivalent resistance safety requirement
        \param LR load resistance: the uniform lateral load that a glass construction can sustain based upon a given probability of breakage and load duration as defined in (pp. 1 and 53) Ref: astm2009 (Pa)
        \param q applied load (demand): 3 second duration equivalent pressure (Pa)
        \return 3 second load equivalent resistance safety requirement
    */
    public static boolean func_isSafeLR(double LR, double q) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_isSafeLR called with inputs: {");
        outfile.print("  LR = ");
        outfile.print(LR);
        outfile.println(", ");
        outfile.print("  q = ");
        outfile.println(q);
        outfile.println("  }");
        outfile.close();
        
        return LR > q;
    }
    
    /** \brief Calculates probability of glass breakage safety requirement
        \param inParams structure holding the input values
        \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
        \return probability of glass breakage safety requirement
    */
    public static boolean func_isSafePb(InputParameters inParams, double P_b) throws IOException {
        PrintWriter outfile;
        outfile = new PrintWriter(new FileWriter(new File("log.txt"), true));
        outfile.println("function func_isSafePb called with inputs: {");
        outfile.print("  inParams = ");
        outfile.print("Instance of InputParameters object");
        outfile.println(", ");
        outfile.print("  P_b = ");
        outfile.println(P_b);
        outfile.println("  }");
        outfile.close();
        
        return P_b < inParams.P_btol;
    }
}
