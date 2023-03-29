## \file Control.py
# \author Jason Balaci
# \brief Controls the flow of the program
import sys

import Calculations
import InputParameters
import OutputFormat

filename = sys.argv[1]
a_0, a_1, a_2, a_3, L_B, E_B, I_B = InputParameters.get_input(filename)
y_B = Calculations.func_y_B(E_B)
OutputFormat.write_output(y_B)
