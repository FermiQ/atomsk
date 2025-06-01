# `crystallography.f90`

## Overview

The `crystallography.f90` module provides a suite of utility subroutines designed to handle common crystallographic calculations and string manipulations related to crystal directions and planes. These include parsing Miller indices from string representations (for both standard cubic/orthorhombic and hexagonal systems), converting between Miller and Miller-Bravais notations for hexagonal lattices, transforming Miller indices into Cartesian vectors based on the cell's basis vectors and orientation, and deriving a chemical formula and its corresponding mass from a list of atomic sites, potentially considering site occupancies.

## Key Components

- **`MODULE crystallography`**: The main container for all crystallographic utility functions.

- **`SUBROUTINE INDEX_MILLER(planestring, planeindices, ifail)`**:
    - Parses a string `planestring` (e.g., "1-10", "[1_-1_0]", "100") representing Miller indices [hkl] into a 3-element real array `planeindices`.
    - Supports compact notation (e.g., "1-10") for single-digit indices and underscore-separated notation (e.g., "12_-15_14") for multi-digit indices. Brackets are optional.
    - `ifail` is an integer output: 0 for success, 1 for a reading error, 2 if forbidden characters (.,:/!) are found.

- **`SUBROUTINE INDEX_MILLER_HCP(planestring, planeindices, ifail)`**:
    - Parses a string `planestring` (e.g., "1-100", "[1_-1_0_0]") representing 4-index Miller-Bravais indices [hkil] for hexagonal systems.
    - Stores the h, k, and l indices into the 3-element real array `planeindices`. The `i` index is used to validate the condition `h+k = -i`.
    - Supports compact and underscore-separated notations. Brackets are optional.
    - `ifail` is an integer output: 0 for success, 1 for a reading error, 2 if `h+k != -i`.

- **`SUBROUTINE HKIL2UVW(h, k, i, l, u, v, w)`**:
    - Converts 4-index Miller-Bravais direction indices `[hkil]` (real inputs, though `i` is implicitly defined by `h` and `k`) for hexagonal systems into 3-index Miller direction indices `[uvw]` (real outputs).
    - The output `[uvw]` indices are reduced to their smallest integer values by dividing by their greatest common divisor.

- **`SUBROUTINE UVW2HKIL(u, v, w, h, k, i, l)`**:
    - Converts 3-index Miller direction indices `[uvw]` (real inputs) into 4-index Miller-Bravais direction indices `[hkil]` (real outputs) for hexagonal systems.
    - The `i` index is calculated as `-(h+k)`.

- **`SUBROUTINE MILLER2VEC(H, dir, ORIENT, vector, ifail)`**:
    - Converts a string `dir` containing Miller or Miller-Bravais indices into a normalized Cartesian unit `vector`.
    - Takes the simulation box basis vectors `H (3x3 REAL(dp))` and the crystal orientation matrix `ORIENT (3x3 REAL(dp))` as input.
    - It first calls `INDEX_MILLER` and, if that fails, `INDEX_MILLER_HCP` to parse `dir`.
    - If parsing is successful, it applies the orientation and converts to a Cartesian vector using the basis `H`, then normalizes the vector.
    - `ifail` is an integer output indicating parsing success/failure.

- **`SUBROUTINE COMPFORMULA(P, AUXNAMES, AUX, formula, mass)`**:
    - Extracts a chemical `formula` (string, e.g., "Si 1 O 2") and calculates the total `mass` (real) of this formula unit from a list of atomic sites `P`.
    - `P (:,4)` contains atomic numbers.
    - Optionally considers site occupancies if an auxiliary property named "occ" is present in `AUXNAMES` and its corresponding values are in `AUX`.
    - Uses `FIND_NSP` to identify unique species and `ATOMSPECIES`/`ATOMMASS` for conversions.

## Important Variables/Constants

- **`planestring (CHARACTER(LEN=*))`**: Input string representing Miller or Miller-Bravais indices.
- **`planeindices (REAL(dp), DIMENSION(3))`**: Output array storing the parsed h, k, l Miller indices.
- **`ifail (INTEGER)`**: Output status indicator for parsing routines (0 for success, non-zero for errors).
- **`h, k, i, l, u, v, w (REAL(dp))`**: Variables representing Miller and Miller-Bravais indices in conversion routines.
- **`H (REAL(dp), DIMENSION(3,3))`**: Input matrix of the three cell basis vectors (a1, a2, a3).
- **`ORIENT (REAL(dp), DIMENSION(3,3))`**: Input crystal orientation matrix, defining how the crystallographic axes [100], [010], [001] are aligned with the Cartesian axes of `H`.
- **`vector (REAL(dp), DIMENSION(3))`**: Output normalized Cartesian vector corresponding to a crystallographic direction.
- **`P (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(IN))`**: Input array of atomic data, where `P(:,4)` holds atomic numbers. Used in `COMPFORMULA`.
- **`AUXNAMES (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(IN))`**: Input array of names for auxiliary atomic properties. Used in `COMPFORMULA` to find "occ" for occupancy.
- **`AUX (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(IN))`**: Input array of auxiliary atomic property values. Used in `COMPFORMULA` for site occupancies.
- **`formula (CHARACTER(LEN=128), INTENT(OUT))`**: Output string representing the chemical formula derived from `P` and `AUX`.
- **`mass (REAL(dp), INTENT(OUT))`**: Output total atomic mass for the derived `formula`.

## Usage Examples

```fortran
USE crystallography
USE atoms ! For ATOMMAXZ, ATOMSPECIES, ATOMMASS
USE comv  ! For dp
IMPLICIT NONE

REAL(dp), DIMENSION(3) :: mill_indices, cart_direction
INTEGER :: error_status
REAL(dp), DIMENSION(3,3) :: cell_H, orientation_matrix
CHARACTER(LEN=16) :: miller_str
REAL(dp) :: h_m, k_m, i_m, l_m, u_m, v_m, w_m

REAL(dp), DIMENSION(2,4) :: atom_P
CHARACTER(LEN=128) :: compound_formula
REAL(dp) :: formula_mass

! Example for INDEX_MILLER
miller_str = "[1_-2_3]"
CALL INDEX_MILLER(miller_str, mill_indices, error_status)
IF (error_status == 0) THEN
  PRINT *, "Parsed Miller [hkl]:", mill_indices ! Should be [1.0, -2.0, 3.0]
END IF

! Example for MILLER2VEC (assuming cell_H and orientation_matrix are defined)
! Let H be an identity matrix (cubic unit cell aligned with Cartesian axes)
cell_H(1,:) = [1.0_dp, 0.0_dp, 0.0_dp]
cell_H(2,:) = [0.0_dp, 1.0_dp, 0.0_dp]
cell_H(3,:) = [0.0_dp, 0.0_dp, 1.0_dp]
! Let orientation be standard ([100]//X, [010]//Y, [001]//Z)
orientation_matrix(1,:) = [1.0_dp, 0.0_dp, 0.0_dp]
orientation_matrix(2,:) = [0.0_dp, 1.0_dp, 0.0_dp]
orientation_matrix(3,:) = [0.0_dp, 0.0_dp, 1.0_dp]
miller_str = "[110]"
CALL MILLER2VEC(cell_H, miller_str, orientation_matrix, cart_direction, error_status)
IF (error_status == 0) THEN
  PRINT *, "Cartesian vector for [110]:", cart_direction ! Should be ~[0.707, 0.707, 0.0]
END IF

! Example for HKIL2UVW
h_m = 1.0_dp; k_m = 0.0_dp; i_m = -1.0_dp; l_m = 0.0_dp
CALL HKIL2UVW(h_m, k_m, i_m, l_m, u_m, v_m, w_m)
PRINT *, "[hkil] ", h_m, k_m, i_m, l_m, " -> [uvw] ", u_m, v_m, w_m ! Should be [2,1,0] for [10-10]

! Example for COMPFORMULA
atom_P(1,:) = [0.0_dp, 0.0_dp, 0.0_dp, 14.0_dp] ! Silicon (Z=14)
atom_P(2,:) = [0.5_dp, 0.5_dp, 0.5_dp, 8.0_dp]  ! Oxygen (Z=8)
! No AUXNAMES or AUX needed if assuming full occupancy and no "occ" property
CALL COMPFORMULA(atom_P, CHARACTER(LEN=128) ::, REAL(dp) ::, compound_formula, formula_mass)
PRINT *, "Formula:", TRIM(compound_formula), "Mass:", formula_mass
! Expected: Formula: Si 1 O 1 Mass: ~44.08 (approx Si + O)
! (Note: COMPFORMULA will determine counts. For SiO2, you'd list 1 Si and 2 O atoms in P)

```

## Dependencies and Interactions

- **`comv`**: Used to provide the `dp` kind parameter for double-precision real numbers.
- **`constants`**: This module is included with `USE constants`, but its specific constants are not directly used in the visible code of these subroutines. It might be used by other routines within the same Atomsk framework.
- **`math`**: The `GCD` (Greatest Common Divisor) function from this module is used in `HKIL2UVW` to simplify the resulting Miller indices.
- **`subroutines`**:
    - `VECLENGTH`: Used in `MILLER2VEC` to normalize the resulting Cartesian vector.
    - `FIND_NSP`: Used in `COMPFORMULA` to identify unique atomic species and their counts from the input atomic data.
- **`atoms` (Module)**: The `COMPFORMULA` subroutine makes calls to:
    - `ATOMSPECIES`: To convert atomic numbers to chemical symbols for constructing the formula string.
    - `ATOMMASS`: To get the mass of each species for calculating the total formula mass.
    - `ATOMMAXZ`: Used as a limit when processing species in `COMPFORMULA`.
- **String Parsing**: `INDEX_MILLER` and `INDEX_MILLER_HCP` perform custom parsing of strings to extract numerical indices, handling signs and optional separators.
- **Crystallographic Conventions**: The routines for Miller and Miller-Bravais conversions adhere to standard crystallographic definitions. The transformation in `MILLER2VEC` correctly applies the cell basis vectors and crystal orientation.
