import numpy as np
import scipy

if __name__ != "__main__":
    print("Please run with 'python main.py'.")
    exit(1)

###############################################################################
# Beam and Loading Options
###############################################################################

# Length of the beam
L = 10

# Young's modulus, 800 might be better?
E = 200000000000

# Moment of inertia
I = 14200

# increasing nodes to 5000 gives an error?! 500 seems to work okay.
nodes = 5

# TODO: Explain the background mathematics

# Uniformly distributed -800 N/m load application
def uniformly_distributed_800Npm_load(x, y):
    return np.vstack((y[1], y[2], y[3], (y[0] - 800) / (E * I)))

# Parabolic distribution, skewed towards middle of beam
def parabolic_distribution(x, y):
    return np.vstack((y[1], y[2], y[3], - (y[0] - L/2) ** 2 / (E * I)))

fun = uniformly_distributed_800Npm_load

# Boundary conditions:
# 1. ends are vertically fixed:   y(0) = 0,   y(L) = 0
# 2. moments at ends are zero:  y''(0) = 0, y''(L) = 0
def bc(ya, yb):
    return np.array([ya[0], yb[0], ya[2], yb[2]])

x = np.linspace(0, L, nodes)
y = np.zeros((4, x.size))

res = scipy.integrate.solve_bvp(fun, bc, x, y, verbose=2, max_nodes=1_000_000)
# Because it's a slender beam, increasing the "tolerance" might be okay, since
# the maximum deflection is already really low (e.g., we might be running into
# floating errors). Is Python really even a good choice for this?

print(res)

if not res.success:
    print("Failed to solve BVP!")
    exit(0)

x_plot = np.linspace(0, L, 100)
y_plot = res.sol(x_plot)[0]
# TODO: Show the solutions of the other derivatives

import matplotlib.pyplot as plt

plt.plot(x_plot, y_plot, label='deflection')
plt.legend()
plt.xlabel("x")
plt.ylabel("y")
plt.show()
