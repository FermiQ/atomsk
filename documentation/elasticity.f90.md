# `elasticity.f90`

## Overview

The `elasticity.f90` module provides a collection of subroutines and functions for handling operations related to elastic tensors. These operations include converting a set of 9 Voigt-notation elastic constants into a 9x9 matrix representation, checking the symmetry of such a 9x9 tensor, mapping between 2-index and 1-index (Voigt-like) notations for this 9x9 matrix, and performing rotations of these 9x9 elastic tensors using a standard 3x3 rotation matrix.

The 9x9 matrix representation used in this module seems to be an extension of the standard 6x6 Voigt notation, and the specific mapping and symmetry operations are particular to this implementation.

## Key Components

- **`MODULE elasticity`**: The main container for the elasticity-related routines.

- **`SUBROUTINE ELAST2TENSOR(elcst, eltens)`**:
    - Converts a 1D array `elcst` of 9 elastic constants (assumed to be C11, C22, C33, C23, C31, C12, C44, C55, C66 in that order) into a 9x9 matrix `eltens`.
    - The routine populates the top-left 6x6 portion of `eltens` according to standard Voigt conventions (e.g., `eltens(1,1)=C11`, `eltens(1,2)=C12`, `eltens(4,4)=C44`).
    - It then fills the remaining parts of the 9x9 matrix by extending symmetry from the 6x6 block (e.g. `eltens(1:3,7:9) = eltens(1:3,4:6)`). This extension is specific to this module's 9x9 representation.

- **`SUBROUTINE CHECK_CTENSOR(C_tensor, status)`**:
    - Checks if a given 9x9 matrix `C_tensor` is symmetric (i.e., `C_tensor(i,j) == C_tensor(j,i)`).
    - Sets the output `status` to 0 if symmetric, and 1 if any asymmetry is detected (within a tolerance of 1.d-3).

- **`FUNCTION ELASTINDEX(i, j) RESULT(m)`**:
    - Converts a pair of Cartesian-like indices `i` and `j` (each ranging from 1 to 3) into a single, Voigt-like index `m` for the 9x9 matrix representation.
    - Mapping:
        - `(i,i) -> i` (e.g., (1,1)->1, (2,2)->2, (3,3)->3)
        - `(1,2)->6`, `(1,3)->8`
        - `(2,1)->9`, `(2,3)->4`
        - `(3,1)->5`, `(3,2)->7`
        - Returns 0 if the (i,j) pair doesn't match these.

- **`FUNCTION ELAST2INDEX(i) RESULT(mn)`**:
    - Converts a single Voigt-like index `i` (from 1 to 9) back into a pair of Cartesian-like indices `mn(1)` and `mn(2)`. This is the inverse of `ELASTINDEX`.
    - Mapping examples: `1->(1,1)`, `4->(2,3)`, `6->(1,2)`.

- **`FUNCTION ROTELAST(ELTENS, T) RESULT(newELTENS)`**:
    - Rotates a 9x9 elastic tensor `ELTENS` using a provided 3x3 rotation matrix `T`.
    - It first constructs a 9x9 transformation matrix `Q` based on `T` and the index mapping defined by `ELAST2INDEX`. The elements of `Q` are `Q(i,j) = T(k,m)*T(l,n)` where `(m,n) = ELAST2INDEX(i)` and `(k,l) = ELAST2INDEX(j)`.
    - The rotated elastic tensor `newELTENS` is then computed using the standard tensor transformation rule: `C' = Q^T * C * Q`.

## Important Variables/Constants

- **`elcst (REAL(dp), DIMENSION(9), INTENT(IN))`**: A 1D array holding 9 elastic constants in Voigt notation (C11, C22, C33, C23, C31, C12, C44, C55, C66).
- **`eltens (REAL(dp), DIMENSION(:,:), INTENT(INOUT))`**: A 9x9 matrix used to store the elastic tensor. Its size must be at least 6x6 for `ELAST2TENSOR`, and 9x9 if the extended symmetry part is used.
- **`C_tensor (REAL(dp), DIMENSION(9,9), INTENT(IN))`**: A 9x9 matrix representing the elastic tensor, used as input for `CHECK_CTENSOR` and `ROTELAST`.
- **`status (INTEGER)`**: Output of `CHECK_CTENSOR`, indicating symmetry (0) or asymmetry (1).
- **`T (REAL(dp), DIMENSION(3,3), INTENT(IN))`**: A 3x3 matrix representing the rotation to be applied to the elastic tensor in `ROTELAST`.
- **`newELTENS (REAL(dp), DIMENSION(9,9))`**: The function result of `ROTELAST`, containing the rotated 9x9 elastic tensor.

## Usage Examples

```fortran
USE elasticity
USE comv ! For dp
IMPLICIT NONE

REAL(dp), DIMENSION(9) :: voigt_C
REAL(dp), DIMENSION(9,9) :: C_matrix_9x9, C_matrix_rotated
REAL(dp), DIMENSION(3,3) :: R_matrix
INTEGER :: sym_status
INTEGER :: m_voigt
INTEGER, DIMENSION(2) :: ij_pair

! Define Voigt elastic constants (e.g., for a cubic material)
! C11, C22, C33, C23, C31, C12, C44, C55, C66
voigt_C = [100.0_dp, 100.0_dp, 100.0_dp, & ! C11, C22, C33
            50.0_dp,  50.0_dp,  50.0_dp, & ! C23, C31, C12
            40.0_dp,  40.0_dp,  40.0_dp]   ! C44, C55, C66

! Convert Voigt constants to 9x9 matrix
CALL ELAST2TENSOR(voigt_C, C_matrix_9x9)
PRINT *, "C_matrix_9x9(1,1) (C11):", C_matrix_9x9(1,1)
PRINT *, "C_matrix_9x9(4,4) (C44):", C_matrix_9x9(4,4)

! Check symmetry
CALL CHECK_CTENSOR(C_matrix_9x9, sym_status)
IF (sym_status == 0) THEN
  PRINT *, "The 9x9 tensor is symmetric."
ELSE
  PRINT *, "The 9x9 tensor is NOT symmetric."
END IF

! Index conversion examples
m_voigt = ELASTINDEX(2,3) ! Should give 4
PRINT *, "ELASTINDEX(2,3):", m_voigt
ij_pair = ELAST2INDEX(m_voigt) ! Should give [2,3]
PRINT *, "ELAST2INDEX(4):", ij_pair

! Rotate the elastic tensor (assuming R_matrix is defined)
! Example: 45-degree rotation around Z-axis
R_matrix(1,:) = [COS(PI/4.0_dp), -SIN(PI/4.0_dp), 0.0_dp]
R_matrix(2,:) = [SIN(PI/4.0_dp),  COS(PI/4.0_dp), 0.0_dp]
R_matrix(3,:) = [0.0_dp,           0.0_dp,        1.0_dp]

C_matrix_rotated = ROTELAST(C_matrix_9x9, R_matrix)
PRINT *, "Rotated C_matrix_9x9(1,1):", C_matrix_rotated(1,1)

END PROGRAM example_elasticity
```

## Dependencies and Interactions

- **`comv`**: Used to define `dp` (double precision kind parameter).
- **`constants`**: This module is included (`USE constants`), likely for mathematical constants such as `PI` (though `PI` is used directly in the example above, it might be defined in `constants` within Atomsk).
- **`math`**: This module is included (`USE math`), but no functions from it (like `GCD`) are directly used in the provided code for `elasticity.f90`.
- **`subroutines`**: This module is included (`USE subroutines`), but no functions from it are directly used in the provided code for `elasticity.f90`.
- **Fortran Intrinsics**: The `ROTELAST` function uses the intrinsic Fortran functions `MATMUL` (for matrix multiplication) and `TRANSPOSE` (to transpose a matrix).
- **Voigt Notation**: The module assumes a specific 9-component Voigt notation (C11, C22, C33, C23, C31, C12, C44, C55, C66) for the input `elcst` in `ELAST2TENSOR`.
- **9x9 Tensor Representation**: The module's operations, particularly `ELAST2TENSOR`'s expansion beyond the 6x6 block and the index mapping in `ELASTINDEX`/`ELAST2INDEX`, are specific to its internal 9x9 representation of the elastic tensor. This is not the standard 3x3x3x3 rank-4 tensor form, nor the standard 6x6 Voigt matrix form.
