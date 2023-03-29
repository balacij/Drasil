## \file InputParameters.py
# \author Jason Balaci
# \brief Provides the function for reading inputs
## \brief Reads input from a file with the given file name
# \param filename name of the input file
# \return coefficient of w_B's term of power 0 (N/m)
# \return coefficient of w_B's term of power 1 (N/m^2)
# \return coefficient of w_B's term of power 2 (N/m^3)
# \return coefficient of w_B's term of power 3 (N/m^4)
# \return length of the beam (m)
# \return modulus of elasticity of the beam (Pa)
# \return moment of second area of a cross-section of the beam (m)
def get_input(filename):
    infile = open(filename, "r")
    infile.readline()
    a_0 = float(infile.readline())
    infile.readline()
    a_1 = float(infile.readline())
    infile.readline()
    a_2 = float(infile.readline())
    infile.readline()
    a_3 = float(infile.readline())
    infile.readline()
    L_B = float(infile.readline())
    infile.readline()
    E_B = float(infile.readline())
    infile.readline()
    I_B = float(infile.readline())
    infile.close()
    
    return a_0, a_1, a_2, a_3, L_B, E_B, I_B
