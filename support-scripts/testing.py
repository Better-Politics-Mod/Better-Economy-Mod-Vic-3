"""
Labor Value Calculator using LU Decomposition
Based on the Better Economy Mod's New Economy Plan

This solves the equation: (I - M^T)^(-1) * L = V
Where:
- M is the input-output matrix (M_ij = amount of good i needed to produce 1 unit of good j)
- L is the vector of direct labor weeks required for each good
- V is the resulting value vector (price in labor weeks)
"""

def lu_decomposition(matrix):
    """
    Performs LU decomposition on a square matrix.
    Returns L and U matrices where A = L * U
    
    Args:
        matrix: n x n square matrix (list of lists)
    
    Returns:
        (L, U): Lower and Upper triangular matrices
    """
    n = len(matrix)
    
    # Initialize L as identity matrix and U as zero matrix
    L = [[0.0 for _ in range(n)] for _ in range(n)]
    U = [[0.0 for _ in range(n)] for _ in range(n)]
    
    # Set diagonal of L to 1
    for i in range(n):
        L[i][i] = 1.0
    
    # Perform LU decomposition using Doolittle's method
    for j in range(n):
        # Calculate U[i][j] for i <= j
        for i in range(j + 1):
            sum_val = 0.0
            for k in range(i):
                sum_val += L[i][k] * U[k][j]
            U[i][j] = matrix[i][j] - sum_val
        
        # Calculate L[i][j] for i > j
        for i in range(j + 1, n):
            sum_val = 0.0
            for k in range(j):
                sum_val += L[i][k] * U[k][j]
            
            if abs(U[j][j]) < 1e-10:
                raise ValueError(f"Matrix is singular or nearly singular at position ({j},{j})")
            
            L[i][j] = (matrix[i][j] - sum_val) / U[j][j]
    
    return L, U


def forward_substitution(L, b):
    """
    Solves L * y = b for y using forward substitution.
    
    Args:
        L: Lower triangular matrix
        b: Right-hand side vector
    
    Returns:
        y: Solution vector
    """
    n = len(b)
    y = [0.0 for _ in range(n)]
    
    for i in range(n):
        sum_val = 0.0
        for j in range(i):
            sum_val += L[i][j] * y[j]
        y[i] = b[i] - sum_val
    
    return y


def backward_substitution(U, y):
    """
    Solves U * x = y for x using backward substitution.
    
    Args:
        U: Upper triangular matrix
        y: Right-hand side vector
    
    Returns:
        x: Solution vector
    """
    n = len(y)
    x = [0.0 for _ in range(n)]
    
    for i in range(n - 1, -1, -1):
        sum_val = 0.0
        for j in range(i + 1, n):
            sum_val += U[i][j] * x[j]
        
        if abs(U[i][i]) < 1e-10:
            raise ValueError(f"Matrix is singular at position ({i},{i})")
        
        x[i] = (y[i] - sum_val) / U[i][i]
    
    return x


def transpose_matrix(matrix):
    """
    Returns the transpose of a matrix.
    
    Args:
        matrix: n x m matrix (list of lists)
    
    Returns:
        Transposed matrix (m x n)
    """
    n = len(matrix)
    m = len(matrix[0])
    
    transposed = [[0.0 for _ in range(n)] for _ in range(m)]
    
    for i in range(n):
        for j in range(m):
            transposed[j][i] = matrix[i][j]
    
    return transposed


def create_identity_matrix(n):
    """
    Creates an n x n identity matrix.
    
    Args:
        n: Size of the matrix
    
    Returns:
        n x n identity matrix
    """
    identity = [[0.0 for _ in range(n)] for _ in range(n)]
    for i in range(n):
        identity[i][i] = 1.0
    return identity


def subtract_matrices(A, B):
    """
    Computes A - B for matrices of the same size.
    
    Args:
        A, B: Matrices to subtract
    
    Returns:
        Result matrix
    """
    n = len(A)
    m = len(A[0])
    
    result = [[0.0 for _ in range(m)] for _ in range(n)]
    
    for i in range(n):
        for j in range(m):
            result[i][j] = A[i][j] - B[i][j]
    
    return result


def calculate_labor_values(M, L):
    """
    Calculates the value of each good according to the formula:
    V = (I - M^T)^(-1) * L
    
    Where:
    - M is the input-output matrix (M_ij = amount of good i to produce 1 unit of good j)
    - L is the vector of direct labor weeks
    - V is the resulting value vector
    
    Args:
        M: Input-output matrix (n x n)
        L: Direct labor vector (length n)
    
    Returns:
        V: Value vector in labor weeks (length n)
    """
    n = len(M)
    
    # Validate inputs
    if len(L) != n:
        raise ValueError(f"Labor vector length {len(L)} doesn't match matrix size {n}")
    
    for row in M:
        if len(row) != n:
            raise ValueError(f"Matrix is not square")
    
    # Step 1: Transpose M to get M^T
    M_transpose = transpose_matrix(M)
    
    # Step 2: Create identity matrix I
    I = create_identity_matrix(n)
    
    # Step 3: Compute (I - M^T)
    A = subtract_matrices(I, M_transpose)
    
    # Step 4: Perform LU decomposition on A
    L_matrix, U_matrix = lu_decomposition(A)
    
    # Step 5: Solve L * y = L_vector using forward substitution
    y = forward_substitution(L_matrix, L)
    
    # Step 6: Solve U * V = y using backward substitution
    V = backward_substitution(U_matrix, y)
    
    return V


def print_matrix(matrix, name="Matrix"):
    """Helper function to print a matrix nicely."""
    print(f"\n{name}:")
    for row in matrix:
        print("  [" + ", ".join(f"{val:8.4f}" for val in row) + "]")


def print_vector(vector, name="Vector"):
    """Helper function to print a vector nicely."""
    print(f"\n{name}:")
    print("  [" + ", ".join(f"{val:8.4f}" for val in vector) + "]")


# Example usage
if __name__ == "__main__":
    # Example: 3-good economy
    # Goods: Steel, Tools, Automobiles
    # M[i][j] = amount of good i needed to produce 1 unit of good j
    
    print("=" * 60)
    print("Labor Value Calculator - Better Economy Mod")
    print("=" * 60)
    
    # Input-output matrix M
    # Rows/Columns: [Steel, Tools, Automobiles]
    M = [
        [0.2,  0.3,  0.5, 0],   # Steel needed for: Steel, Tools, Automobiles
        [0.1,  0.1,  0.2, 0],   # Tools needed for: Steel, Tools, Automobiles
        [0.0,  0.0,  0.1, 0],    # Automobiles needed for: Steel, Tools, Automobiles
        [0.0,  0.0,  0.0, 0]
    ]
    
    # Direct labor weeks required for each good
    L = [2.0, 3.0, 5.0, 0.0]  # [Steel, Tools, Automobiles]
    
    print("\nInput-Output Matrix M:")
    print("(M[i][j] = amount of good i needed to produce 1 unit of good j)")
    print_matrix(M, "M")
    
    print("\nDirect Labor Weeks Vector L:")
    print_vector(L, "L")
    
    # Calculate labor values
    V = calculate_labor_values(M, L)
    
    print("\n" + "=" * 60)
    print("RESULTS")
    print("=" * 60)
    print("\nTotal Labor Value Vector V (in Labor Weeks):")
    print_vector(V, "V")
    
    print("\n" + "-" * 60)
    print("Interpretation:")
    print("-" * 60)
    goods = ["Steel", "Tools", "Automobiles", "Explosives"]
    for i, good in enumerate(goods):
        print(f"{good:15s}: {V[i]:.4f} Labor Weeks per unit")
    
    print("\n" + "=" * 60)
    print("\nThis represents the total embodied labor (direct + indirect)")
    print("required to produce one unit of each good.")
    print("=" * 60)