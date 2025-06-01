# `math.f90`

## Overview

The `math.f90` module is a library of mathematical functions and subroutines designed to support various calculations within the Atomsk program. It includes a range of operations from basic vector algebra (length, angles, dot/cross products) and matrix manipulations (trace, determinant, inverse, rotation matrices from axis/angle or Euler angles) to number theory utilities (Greatest Common Divisor) and geometric calculations (volume of parallelepiped, point-plane relationship). It also provides conversion routines between different representations of crystal cell parameters and a simple numerical derivative function.

## Key Components

### Functions

- **`IS_INTEGER(number, th)`**: Checks if a `REAL(dp)` `number` is effectively an integer within a given tolerance `th`. Returns `.TRUE.` or `.FALSE.`.
- **`VECLENGTH(V)`**: Calculates and returns the Euclidean length (magnitude) of a 3D vector `V`.
- **`VEC_PLANE(N, d0, P)`**: Determines the position of a point `P` relative to a plane defined by its normal vector `N` and distance from origin `d0`. Returns a positive value if above, negative if below, zero if on the plane.
- **`GCD(n, m)`**: Recursively calculates the Greatest Common Divisor (GCD) of two integers `n` and `m`.
- **`ANGVEC(V1, V2)`**: Computes the angle (in radians) between two 3D vectors `V1` and `V2` using their dot product.
- **`VEC_ANGLE(u1, u2, n)`**: Computes the oriented angle (in radians) between vectors `u1` and `u2`, with the orientation determined by the normal vector `n` (using the scalar triple product).
- **`VECMAT(u1, u2)`**: Computes the outer product of two 3D vectors `u1` (treated as a column vector) and `u2` (treated as a row vector) to produce a 3x3 matrix `m(i,j) = u1(i)*u2(j)`.
- **`DEG2RAD(angdeg)`**: Converts an angle from degrees (`angdeg`) to radians.
- **`RAD2DEG(angrad)`**: Converts an angle from radians (`angrad`) to degrees.
- **`ORTHOVEC(V1, V2)`**: Returns `.TRUE.` if the two 3D vectors `V1` and `V2` are orthogonal (dot product is zero), `.FALSE.` otherwise.
- **`CROSS_PRODUCT(V1, V2)`**: Calculates the cross product of two 3D vectors `V1` and `V2`, returning the resultant 3D vector.
- **`SCALAR_TRIPLE_PRODUCT(ex, ey, ez)`**: Computes the scalar triple product `ez . (ex x ey)` for three 3D vectors.
- **`ROTMAT_AXIS(axis, angle)`**: Generates a 3x3 rotation matrix for a rotation of `angle` (in degrees) around a given 3D `axis`.
- **`ROTMAT_VECTORS(a, b)`**: Generates a 3x3 rotation matrix that rotates vector `a` onto vector `b`.
- **`EPS_LEVI_CIVITA(i, j, k)`**: Computes the Levi-Civita symbol (permutation symbol) for indices `i, j, k`. Returns 1 for even permutations of (1,2,3), -1 for odd permutations, and 0 if any indices are repeated.
- **`MATTRACE(A)`**: Computes the trace (sum of diagonal elements) of a square matrix `A`.
- **`MATDET(A)`**: Computes the determinant of a 3x3 matrix `A`.
- **`IS_ROTMAT(matrix)`**: Checks if a given 3x3 `matrix` is a valid rotation matrix by verifying if `matrix * TRANSPOSE(matrix)` is close to the identity matrix. Returns `.TRUE.` or `.FALSE.`.

### Subroutines

- **`INVMAT(M, G, status)`**: Computes the inverse of a square matrix `M`, storing the result in `G`. For 3x3 matrices, it uses an explicit formula. For general NxN matrices, it uses LAPACK routines (`DGETRF` and `DGETRI`). `status` (optional) returns 0 on success.
- **`CONVMAT(a, b, c, alpha, beta, gamma, H)`**: Converts conventional lattice parameters (lengths `a,b,c` and angles `alpha,beta,gamma` in radians) into a lower triangular 3x3 cell matrix `H`. This representation is common for simulation boxes (e.g., LAMMPS triclinic cell).
- **`MATCONV(H, a, b, c, alpha, beta, gamma)`**: Converts a 3x3 cell matrix `H` into conventional lattice parameters (lengths `a,b,c` and angles `alpha,beta,gamma` in radians).
- **`VOLUME_PARA(Pvec, Volume)`**: Computes the `Volume` of a parallelepiped defined by the three basis vectors stored as rows in the 3x3 matrix `Pvec`. It internally uses `MATCONV`.
- **`DERIVATIVE(func, dfunc)`**: Calculates the centered finite difference derivative of a 1D function represented by `func(:,2)` with respect to `func(:,1)`. The result `dfunc(:,2)` is stored with corresponding x-values in `dfunc(:,1)`.
- **`EULER2MAT_ZYX(a, b, c, rotmat)`**: Converts Euler angles `a, b, c` (in radians, ZYX convention: rotation around X, then Y, then Z) into a 3x3 rotation matrix `rotmat`.
- **`MAT2EULER_ZYX(rotmat, a, b, c)`**: Converts a 3x3 rotation matrix `rotmat` into Euler angles `a, b, c` (in radians, ZYX convention).

## Important Variables/Constants

This module primarily provides functions and subroutines. It does not define public constants itself but relies on `pi` from the `constants` module. Key inputs and outputs are vectors (typically `REAL(dp), DIMENSION(3)`) and matrices (typically `REAL(dp), DIMENSION(3,3)` or `REAL(dp), DIMENSION(:,:)`).

## Usage Examples

```fortran
USE math
USE comv      ! For dp
USE constants ! For pi
IMPLICIT NONE

REAL(dp) :: len, angle_rad, angle_deg, det_A, trace_A
REAL(dp), DIMENSION(3) :: vec1 = [1.0_dp, 0.0_dp, 0.0_dp]
REAL(dp), DIMENSION(3) :: vec2 = [0.0_dp, 1.0_dp, 0.0_dp]
REAL(dp), DIMENSION(3) :: cross_p_vec
REAL(dp), DIMENSION(3,3) :: mat_A, mat_inv_A, rot_M
INTEGER :: gcd_val, stat
LOGICAL :: is_int_check, is_ortho_check

len = VECLENGTH(vec1)               ! len = 1.0
angle_rad = ANGVEC(vec1, vec2)      ! angle_rad = pi/2
angle_deg = RAD2DEG(angle_rad)      ! angle_deg = 90.0

CALL CROSS_PRODUCT(vec1, vec2, cross_p_vec) ! cross_p_vec = [0,0,1]

mat_A = reshape([1.0_dp, 2.0_dp, 3.0_dp, &
                 0.0_dp, 1.0_dp, 4.0_dp, &
                 5.0_dp, 6.0_dp, 0.0_dp], [3,3])
det_A = MATDET(mat_A)               ! det_A = 1.0
trace_A = MATTRACE(mat_A)           ! trace_A = 2.0

CALL INVMAT(mat_A, mat_inv_A, stat) ! Inverts mat_A into mat_inv_A

CALL ROTMAT_AXIS([0.0_dp, 0.0_dp, 1.0_dp], 90.0_dp, rot_M) ! Rotation matrix around Z by 90 deg

is_int_check = IS_INTEGER(5.00000000000001_dp, 1.0d-12) ! TRUE

gcd_val = GCD(48, 18) ! gcd_val = 6
```

## Dependencies and Interactions

- **`comv`**: Provides the `dp` kind parameter for double-precision real numbers.
- **`constants`**: Provides the value of `pi`.
- **LAPACK**: The `INVMAT` subroutine uses LAPACK routines `DGETRF` (LU decomposition) and `DGETRI` (matrix inverse from LU decomposition) for general NxN matrix inversion. This implies a dependency on a linked LAPACK library for full functionality beyond 3x3 matrices.
- **Fortran Intrinsic Functions**: The module extensively uses Fortran intrinsic functions for mathematical operations (e.g., `DSQRT`, `DABS`, `DOT_PRODUCT`, `DACOS`, `DSIN`, `DCOS`, `MODULO`, `NINT`, `MAX`, `MIN`, `MATMUL`, `TRANSPOSE`, `SIGN`, `RESHAPE`).
