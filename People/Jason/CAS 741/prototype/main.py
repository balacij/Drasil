import numpy as np
import scipy

if __name__ != "__main__":
    print("main.py is not a valid import.")
    exit(1)

L = 10 # Length of the beam
nodes = 5 # increasing nodes to 5000 gives an error?! 500 seems to work okay.
E = 80 # Young's modulus
I = 16 # Moment of inertia

def fun(x, y):
    return np.vstack((y[1], - (y[0] - L/2) ** 2 / (E * I)))

def bc(ya, yb):
    return np.array([ya[0], yb[0]])

x = np.linspace(0, L, nodes)
y = np.zeros((2, x.size))

res = scipy.integrate.solve_bvp(fun, bc, x, y, verbose=2, max_nodes=1000000)

print(res)

if not res.success:
    print("Failed to solve BVP!")
    exit(0)

x_plot = np.linspace(0, L, 100)
y_plot = res.sol(x_plot)[0]

import matplotlib.pyplot as plt

plt.plot(x_plot, y_plot, label='deflection')
plt.legend()
plt.xlabel("x")
plt.ylabel("y")
plt.show()
