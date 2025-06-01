# `symops.f90`

## Overview

The `symops.f90` module is responsible for handling and applying crystallographic symmetry operations within Atomsk. A symmetry operation is defined in terms of a 3x3 transformation matrix (M) and a 3D shift (translation) vector (S), such that a point P is transformed to P' = M*P + S. This module provides routines to parse human-readable symmetry operation strings (e.g., "x-y+1/2, x, z"), convert them into this internal numerical representation, store them, and then apply these operations to a set of atomic coordinates and their associated properties. It works in conjunction with the `spacegroups` module to obtain the necessary symmetry operations for a given space group and generate a full crystal cell from an asymmetric unit.

## Key Components

- **`MODULE symops`**: The main module for symmetry operations.

- **Module Data**:
    - **`symops_chanstr (CHARACTER(LEN=3), PARAMETER, PUBLIC)`**: Defines the characters representing coordinate channels, typically "xyz".
    - **`symops_nltrf (INTEGER, PARAMETER, PUBLIC)`**: Value 12. Specifies the number of `REAL(dp)` values used to store one symmetry operation (3 for the shift vector S, and 9 for the 3x3 matrix M).
    - **`symops_trf (REAL(dp), DIMENSION(:,:), ALLOCATABLE, PUBLIC)`**: A 2D array, `symops_trf(12, N_ops)`, that stores `N_ops` symmetry operations. Each column represents one operation: elements 1-3 are the shift vector (S_x, S_y, S_z), and elements 4-12 are the matrix M row by row (M_xx, M_xy, M_xz, M_yx, ... M_zz).

- **Subroutines**:
    - **`SYMOPS_INIT()`**: Initializes an already allocated `symops_trf` array. Each column is set to represent an identity transformation (zero shift vector, identity matrix).
    - **`SYMOPS_APPLY(H, P, S_shells, AUXNAMES, AUX, dmindist, nchk)`**: Applies all symmetry operations currently stored in the module's `symops_trf` array to the input atomic positions `P`. It also transforms shell positions `S_shells` (if present and correctly dimensioned) and auxiliary properties `AUX` (if present).
        - Atoms are transformed, converted to fractional coordinates using cell matrix `H`, wrapped into the [0,1) unit cell.
        - Duplicate atoms (those falling closer than `dmindist` to an existing atom of the same species after transformation and wrapping) are detected and handled (typically, only unique sites are kept, potentially summing occupancies if the "occ" property exists).
        - The input arrays `P`, `S_shells`, and `AUX` are reallocated to accommodate the newly generated atomic sites.
        - `nchk (INTEGER, INTENT(OUT))` returns 1 for success, 0 for failure.
    - **`SYMOPS_CHECK_STR(instr, nchk)`**: Validates if a given input string `instr` conforms to the expected format of a symmetry operation string (e.g., "x,y,z" or "-y,x-y+1/2,z"). Checks for comma count and allowed characters. `nchk` is 1 if valid, 0 otherwise.
    - **`SYMOPS_SET_STR(instr, irow, nchk)`**: Parses a single symmetry operation string `instr` and converts it into its numerical representation (12 real numbers), storing it in the `irow`-th column of the global `symops_trf` array. It uses `SYMOPS_PARSE_STR_LINTRF` for parsing each component of the string. `nchk` indicates success (1) or failure (0).
    - **`RECURSIVE SUBROUTINE SYMOPS_PARSE_STR_LINTRF(instr, shift, slope, nchk)`**: A recursive helper routine that parses one component of a symmetry operation string (e.g., "x-y+1/2"). It extracts the constant shift part (e.g., 1/2) and the coefficients for x, y, and z (e.g., 1 for x, -1 for y, 0 for z).
        - `shift (REAL(dp), INTENT(OUT))`: The translational part.
        - `slope (REAL(dp), DIMENSION(3), INTENT(OUT))`: The coefficients for x, y, z in the matrix part.
        - `nchk` indicates success.
    - **`SYMOPS_SET_SGNAME(sgname, nchk)`**: Populates `symops_trf` with all symmetry operations for a space group specified by its Hermann-Mauguin symbol `sgname`. It internally calls `SG_NAMGETNUM` (from `spacegroups` module) to get the space group number, then calls `SYMOPS_SET_SGNUM`.
    - **`SYMOPS_SET_SGNUM(nsgnumber, nchk)`**: Populates `symops_trf` for a space group given by its `nsgnumber` (1-230). It retrieves the symmetry operation strings from the `spacegroups` module (using `SG_NUMGETSYMOPS`) and then calls `SYMOPS_SET_STR` for each string.
    - **`SG_APPLY_SYMOPS(sgroup, H, P, S_shells, AUXNAMES, AUX)`**: A high-level convenience routine. It takes a space group identifier `sgroup` (either H-M name or number), retrieves all its symmetry operations (which populates the internal `symops_trf`), and then calls `SYMOPS_APPLY` to apply these operations to the provided atomic data (`P`, `S_shells`, `AUX`). This is typically used to generate a full unit cell from an asymmetric set of atoms.

## Important Variables/Constants

- **`symops_trf (REAL(dp), DIMENSION(:,:), ALLOCATABLE, PUBLIC)`**: The central data structure of the module. It's a 2D array where each column stores one symmetry operation as 12 real numbers: 3 for the shift vector and 9 for the transformation matrix elements. This array is populated by routines like `SYMOPS_SET_STR`, `SYMOPS_SET_SGNAME`, or `SYMOPS_SET_SGNUM`, and then used by `SYMOPS_APPLY`.

## Usage Examples

Generating a crystal structure using space group symmetry:
```fortran
USE symops
USE spacegroups ! For SG_INIT, and because symops calls it.
USE comv        ! For dp, nerr, etc.
IMPLICIT NONE

CHARACTER(LEN=32) :: space_group_symbol
REAL(dp), DIMENSION(3,3) :: cell_vectors
REAL(dp), DIMENSION(:,:), ALLOCATABLE :: atoms, shells, aux_properties
CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: aux_names
INTEGER :: init_status

! 1. Initialize the spacegroups module (contains the database of operations)
CALL SG_INIT(init_status)
IF (init_status /= 1) THEN
  PRINT *, "Failed to initialize spacegroups database."
  STOP
END IF

! 2. Define the asymmetric unit's atoms and cell parameters
! Example: Diamond (Fd-3m, No. 227), one atom in asymmetric unit
space_group_symbol = "Fd-3m" ! Or "227"
ALLOCATE(atoms(1,4))
atoms(1,:) = [0.0_dp, 0.0_dp, 0.0_dp, 6.0_dp] ! Carbon at (0,0,0)
cell_vectors(1,:) = [3.567_dp, 0.0_dp, 0.0_dp]
cell_vectors(2,:) = [0.0_dp, 3.567_dp, 0.0_dp]
cell_vectors(3,:) = [0.0_dp, 0.0_dp, 3.567_dp]

! shells and aux_properties can be unallocated if not used
! ALLOCATE(shells(0,0)), ALLOCATE(aux_properties(0,0)), ALLOCATE(aux_names(0))

! 3. Apply symmetry operations
! This will:
!   a) Look up "Fd-3m" in the spacegroups module.
!   b) Populate symops_trf with the 192 operations for Fd-3m.
!   c) Apply these operations to 'atoms', generating new atoms,
!      handling duplicates, and reallocating 'atoms' to the final size.
CALL SG_APPLY_SYMOPS(space_group_symbol, cell_vectors, atoms, shells, aux_names, aux_properties)

IF (nerr == 0) THEN
  PRINT *, "Successfully applied space group operations for ", TRIM(space_group_symbol)
  PRINT *, "Number of atoms in the full cell: ", SIZE(atoms,1) ! Should be 8 for conventional diamond cell
  ! Atoms array now contains all atoms in the conventional cell.
ELSE
  PRINT *, "Error applying space group operations."
END IF

! Clean up
IF (ALLOCATED(atoms)) DEALLOCATE(atoms)
IF (ALLOCATED(symops_trf)) DEALLOCATE(symops_trf) ! symops_trf is module data
CALL SG_UNINIT()

END PROGRAM example_sg_apply
```

## Dependencies and Interactions

- **`comv`**: Provides global parameters like `dp` (double precision kind), `verbosity` level, and error/warning counters (`nerr`, `nwarn`).
- **`constants`**: Included via `USE constants`, likely for fundamental constants if needed by deeper logic, though not directly evident in the interfaces.
- **`messages`**: Uses `ATOMSK_MSG` for outputting messages, warnings, and errors.
- **`files`**: `USE files` is present, but its direct utility within this module is not apparent from the provided code.
- **`subroutines`**: Relies on:
    - `CART2FRAC` and `FRAC2CART` for coordinate transformations during the application of symmetry operations and duplicate checking.
    - `MATCONV` and `INVMAT` for cell matrix operations.
    - `VECLENGTH` for distance calculations (duplicate checking).
- **`spacegroups`**: This is a critical dependency. The `symops` module calls routines from the `spacegroups` module (e.g., `SG_NAMGETNUM`, `SG_NUMGETSYMNUM`, `SG_NUMGETSYMOP`, `SG_NUMGETSYMOPS`) to retrieve the list of symmetry operation strings for a given space group. The `spacegroups` module itself contains the hardcoded database of these operations.
- **Recursion**: The `SYMOPS_PARSE_STR_LINTRF` subroutine is recursive, used to break down complex symmetry operation strings into their constituent parts.
- **Error Handling**: Routines generally use an `nchk` output parameter to signal success (1) or failure (various negative values or 0). Some routines also increment the global `nerr` counter from the `comv` module.
