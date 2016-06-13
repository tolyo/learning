# 6.00 Problem Set 2
#
# Successive Approximation
#

def evaluate_poly(poly, x):
    total = 0.00
    for i in poly:
        total += i * (x ** poly.index(i))
    return total
    """
    Computes the polynomial function for a given value x. Returns that value.

    Example:

    180339.9

    poly: tuple of numbers, length > 0
    x: number
    returns: float
    """
    # TO DO ... 

# poly = (0.0, 0.0, 5.0, 9.3, 7.0)    # f(x) = 7x^4 + 9.3x^3 + 5x^2
# x = -13
# print evaluate_poly(poly, x)  # f(-13) = 7(-13)^4 + 9.3(-13)^3 + 5(-13)^2

def compute_deriv(poly):
    deriv = ()
    for i in poly:
        if (poly.index(i) == 0):
            continue
        else:
            deriv = deriv + (i * poly.index(i),)
    return deriv
    """
    Computes and returns the derivative of a polynomial function. If the
    derivative is 0, returns (0.0,).

    Example:
    (0.0, 35.0, 9.0, 4.0)

    poly: tuple of numbers, length > 0
    returns: tuple of numbers
    """
    # TO DO ...
#poly = (-13.39, 0.0, 17.5, 3.0, 1.0)    # x^4 + 3x^3 + 17.5x^2 - 13.39
#print compute_deriv(poly)        # 4x^3 + 9x^2 + 35^x

def compute_root(poly, x_0, epsilon):
    total = 0
    while True:
        result = evaluate_poly(poly, x_0)
        if abs(result) < epsilon:
            total += 1
            break
        else:
            total += 1
            x_0 = x_0 - result/evaluate_poly(compute_deriv(poly), x_0)
    return (x_0, total)


    """
    Uses Newton's method to find and return a root of a polynomial function.
    Returns a tuple containing the root and the number of iterations required
    to get to the root.

    Example:
    poly = (-13.39, 0.0, 17.5, 3.0, 1.0)    #x^4 + 3x^3 + 17.5x^2 - 13.39
    x_0 = 0.1
    epsilon = .0001
    print compute_root(poly, x_0, epsilon)
    (0.80679075379635201, 8.0)

    poly: tuple of numbers, length > 1.
         Represents a polynomial function containing at least one real root.
         The derivative of this polynomial function at x_0 is not 0.
    x_0: float
    epsilon: float > 0
    returns: tuple (float, int)
    """
poly = (-13.39, 0.0, 17.5, 3.0, 1.0)    #x^4 + 3x^3 + 17.5x^2 - 13.39
x_0 = 0.1
epsilon = .0001
print compute_root(poly, x_0, epsilon)
#(0.80679075379635201, 8.0)

