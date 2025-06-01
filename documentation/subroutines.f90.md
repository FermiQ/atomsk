# `subroutines.f90`

## Overview

The `subroutines.f90` module is a utility library within Atomsk, containing a collection of general-purpose subroutines that perform various common tasks. These tasks include memory allocation checks, validation of data array consistency (e.g., between atom positions, shells, and auxiliary properties), detection of Not-a-Number (NaN) values in arrays, basic statistical analysis of 1D arrays, coordinate transformations between Cartesian and fractional systems, counting atoms outside the primary simulation cell, identifying unique atomic species and their counts within a system, attempting to heuristically determine if coordinates are already in a reduced (fractional) format, and unwrapping atomic coordinates that have been subjected to periodic boundary conditions.

## Key Components

- **`MODULE subroutines`**: The main module housing these utility routines.

- **`SUBROUTINE CHECKMEM(Asize, status)`**:
    - Checks if it's feasible to allocate a `REAL(dp)` array of `Asize` x 4 elements.
    - `Asize (REAL(dp), INTENT(IN))`: The desired number of rows (atoms).
    - `status (INTEGER, INTENT(OUT))`: 0 if OK; 1 if `Asize` exceeds `NATOMS_MAX` (from `comv`) or is non-positive; 2 if a test allocation fails (not enough memory).

- **`SUBROUTINE CHECK_ARRAY_CONSISTENCY(P, S, AUX, AUXNAMES, status)`**:
    - Validates that the dimensions of shell array `S`, auxiliary property array `AUX`, and auxiliary property names array `AUXNAMES` are consistent with the main atom array `P` and with each other.
    - `status (INTEGER, INTENT(OUT))`: 0 if consistent; 1 if `S` dimensions are problematic; 2 if `AUX` dimensions relative to `P` are problematic; 3 if `AUXNAMES` size doesn't match `AUX`'s second dimension. Also deallocates zero-sized arrays.

- **`SUBROUTINE CHECKNAN(A, NaNindex)`**:
    - Scans a 2D `REAL(dp)` array `A` for NaN (Not a Number) or Infinity values.
    - `NaNindex (INTEGER, INTENT(OUT))`: Returns the row index of the first occurrence of NaN/Inf. If no such values are found, `NaNindex` is 0.

- **`SUBROUTINE DO_STATS(array, mi, M, A_avg, D_dev, S_stddev)`**:
    - Calculates basic descriptive statistics for a 1D `REAL(dp)` `array`.
    - **Outputs**: `mi` (minimum value), `M` (maximum value), `A_avg` (average), `D_dev` (mean absolute deviation), `S_stddev` (standard deviation).

- **`SUBROUTINE CART2FRAC(A, H)`**:
    - Converts Cartesian coordinates stored in the first 3 columns of array `A` into fractional (reduced) coordinates. The transformation is done in-place.
    - `H (REAL(dp), DIMENSION(3,3), INTENT(IN))`: The 3x3 matrix of cell basis vectors.

- **`SUBROUTINE FRAC2CART(A, H)`**:
    - Converts fractional coordinates in array `A` (first 3 columns) into Cartesian coordinates. The transformation is done in-place.
    - `H (REAL(dp), DIMENSION(3,3), INTENT(IN))`: The 3x3 matrix of cell basis vectors.

- **`SUBROUTINE COUNT_OUTBOX(H, A, Nout)`**:
    - Counts the number of atoms (`Nout`) in array `A` whose fractional coordinates (calculated using cell matrix `H`) fall outside the range [0,1) in any of the x, y, or z directions.

- **`SUBROUTINE FIND_NSP(A, aentries)`**:
    - Identifies the unique entries (typically atomic numbers) in a 1D `REAL(dp)` array `A`.
    - **Output**: `aentries (REAL(dp), DIMENSION(:,:), ALLOCATABLE)` - A 2D array where `aentries(j,1)` is the j-th unique value found in `A`, and `aentries(j,2)` is its count (how many times it appeared in `A`).

- **`SUBROUTINE FIND_IF_REDUCED(H, array, isreduced)`**:
    - Heuristically determines if the coordinates in `array` (first 3 columns) are likely already in fractional (reduced) form.
    - `isreduced (LOGICAL, INTENT(OUT))`: Set to `.TRUE.` if coordinates appear to be fractional, `.FALSE.` otherwise.
    - The determination is based on whether values are predominantly within [0,1) or if their spread (min/max difference, standard deviation) is small relative to cell dimensions `H`.

- **`SUBROUTINE UNWRAP(atoms_v, dir, vector, threshold, nwrapat)`**:
    - Adjusts coordinates in `atoms_v(:,dir)` to "unwrap" atoms that have crossed periodic boundaries.
    - `dir (INTEGER)`: The direction (1 for x, 2 for y, 3 for z) to unwrap.
    - `vector (REAL(dp))`: The magnitude of the cell vector along `dir`.
    - `threshold (REAL(dp))`: If the distance between consecutive atoms in the `dir` direction exceeds this, unwrapping is attempted.
    - `nwrapat (INTEGER, INTENT(OUT))`: Counts the number of atoms whose positions were modified.
    - It iterates through atoms, and if `atoms_v(i,dir) - atoms_v(i-1,dir)` is too large/small, it subtracts/adds `vector` from `atoms_v(i,dir)`.

## Important Variables/Constants

- **`NATOMS_MAX (INTEGER)`**: A global constant imported from the `comv` module, representing the maximum number of atoms Atomsk can handle. Used in `CHECKMEM`.
- **`nerr (INTEGER)`**: A global error counter from `comv`, incremented by `CART2FRAC` and `FRAC2CART` if array dimensions are inconsistent.

## Usage Examples

```fortran
USE subroutines
USE comv ! For dp, nerr, NATOMS_MAX
USE math ! For INVMAT, VECLENGTH (though VECLENGTH is also intrinsic in math module)
IMPLICIT NONE

REAL(dp), DIMENSION(10,4) :: atom_coords_cart, atom_coords_frac
REAL(dp), DIMENSION(3,3) :: cell_matrix
INTEGER :: status_ok, num_outside
REAL(dp), DIMENSION(5) :: data_series
REAL(dp) :: d_min, d_max, d_avg, d_dev, d_std
LOGICAL :: coords_are_reduced

! Initialize cell_matrix (e.g., for a 10x10x10 cubic box)
cell_matrix = 0.0_dp
cell_matrix(1,1) = 10.0_dp
cell_matrix(2,2) = 10.0_dp
cell_matrix(3,3) = 10.0_dp

! Initialize atom_coords_cart with some Cartesian coordinates
! ... (fill atom_coords_cart) ...

! Check memory for a hypothetical large system
CALL CHECKMEM(1.0d9, status_ok) ! Check for 1 billion atoms
IF (status_ok /= 0) PRINT *, "Memory check status:", status_ok

! Convert to fractional
atom_coords_frac = atom_coords_cart
CALL CART2FRAC(atom_coords_frac, cell_matrix)

! Count atoms outside [0,1) in fractional (should be 0 if wrapped)
CALL COUNT_OUTBOX(cell_matrix, atom_coords_frac, num_outside)
PRINT *, "Atoms outside [0,1) after CART2FRAC:", num_outside

! Convert back to Cartesian
CALL FRAC2CART(atom_coords_frac, cell_matrix)
! atom_coords_frac should now be very close to original atom_coords_cart

! Statistics on x-coordinates
data_series = atom_coords_cart(1:5,1) ! Take x-coords of first 5 atoms
CALL DO_STATS(data_series, d_min, d_max, d_avg, d_dev, d_std)
PRINT *, "Stats for x-coords: Min=", d_min, "Max=", d_max, "Avg=", d_avg

! Check if coordinates are reduced
CALL FIND_IF_REDUCED(cell_matrix, atom_coords_frac, coords_are_reduced)
PRINT *, "Are atom_coords_frac reduced? ", coords_are_reduced ! Expected: .TRUE.
CALL FIND_IF_REDUCED(cell_matrix, atom_coords_cart, coords_are_reduced)
PRINT *, "Are atom_coords_cart reduced? ", coords_are_reduced ! Likely .FALSE.

END PROGRAM example_subroutines
```

## Dependencies and Interactions

- **`comv`**: Provides access to global variables like `dp` (double precision kind), `NATOMS_MAX` (maximum atom count), and `nerr` (global error counter).
- **`constants`**: `USE constants` is present, likely for physical or mathematical constants if needed by any deeper calls, though not directly used in the surface logic of these subroutines.
- **`selection`**: The `USE selection` statement is present, but no routines from it are directly called within this module's code.
- **`math`**:
    - `INVMAT` (from `math` module) is used by `CART2FRAC` to get the inverse of the cell matrix.
    - `VECLENGTH` (from `math` module, also a Fortran intrinsic if module not present) is used by `FIND_IF_REDUCED`.
- **Fortran Intrinsic Functions**: The module utilizes various intrinsic functions, including `NINT`, `DABS`, `ALLOCATE`, `DEALLOCATE`, `SIZE`, `MAXVAL`, `MINVAL`, `ANY`, `SUM`, `DSQRT`, `TRIM`, `ADJUSTL`, `SCAN`.
- **Error Handling**: Some routines like `CART2FRAC`, `FRAC2CART`, and `CHECKMEM` modify the global `nerr` variable or return a status flag to indicate issues.
