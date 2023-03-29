## \file OutputFormat.py
# \author Jason Balaci
# \brief Provides the function for writing outputs
## \brief Writes the output values to output.txt
# \param y_B deflection at a particular point along the beam (m)
def write_output(y_B):
    outputfile = open("output.txt", "w")
    print("y_B = ", end="", file=outputfile)
    print(y_B, file=outputfile)
    outputfile.close()
