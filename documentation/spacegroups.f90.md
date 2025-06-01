# `spacegroups.f90`

## Overview

The `spacegroups.f90` module serves as a comprehensive database and an Application Programming Interface (API) for accessing crystallographic space group symmetry information. It contains data for all 230 standard space groups, including their Hermann-Mauguin (H-M) symbols, corresponding Patterson space group numbers, the total number of symmetry operations for each space group, and the explicit string representations of these symmetry operations (e.g., "x,y,z", "-x,y+1/2,-z").

The module is designed for on-demand initialization: the large dataset of symmetry operations is loaded into memory only when first requested by one of the data access routines or when explicitly initialized via `SG_INIT`. This helps in managing memory efficiently. A corresponding `SG_UNINIT` routine allows for deallocation of this data. A suite of getter functions provides access to the space group data either by the space group number (1-230) or by its H-M symbol.

## Key Components

- **`MODULE spacegroups`**: The main module.

- **Constants**:
    - **`sg_nummax (PUBLIC, PARAMETER, INTEGER)`**: Value 230, representing the total number of space groups.
    - **`sg_opmax (PRIVATE, PARAMETER, INTEGER)`**: Value 192, the maximum number of symmetry operations any single space group can have.
    - **`sg_soplen (PUBLIC, PARAMETER, INTEGER)`**: Value 32, the fixed character length for storing symmetry operation strings.

- **Private Data Arrays (Allocatable)**:
    - `sg_name(:) (CHARACTER(len=sg_soplen))`: Stores the Hermann-Mauguin symbols for each space group.
    - `sg_symmetry(:,:) (CHARACTER(len=sg_soplen))`: A 2D array `sg_symmetry(op, sg)` storing the string for the `op`-th symmetry operation of space group `sg`.
    - `sg_patn(:) (INTEGER)`: Stores the Patterson space group number for each space group.
    - `sg_symnum(:) (INTEGER)`: Stores the total number of symmetry operations for each space group.

- **Initialization and Status Routines**:
    - **`SUBROUTINE SG_INIT(state)`**: Allocates and populates the private data arrays with the hardcoded information for all 230 space groups. `state (INTEGER, INTENT(OUT))` returns 1 on successful initialization, 0 on failure.
    - **`SUBROUTINE SG_UNINIT()`**: Deallocates all the private data arrays, freeing up memory.
    - **`SUBROUTINE SG_ISREADY(state)`**: Checks if the module's data arrays are currently allocated. `state (INTEGER, INTENT(OUT))` is 1 if initialized, 0 otherwise. Most data access routines call this and trigger `SG_INIT` if necessary.

- **Data Access Routines (by Space Group Number `nsgnum`)**:
    Each of these routines takes `nsgnum (INTEGER, INTENT(IN))` and returns data, plus an error/success code `nchk (INTEGER, INTENT(OUT))` (1 for success, <=0 for errors).
    - **`SG_NUMGETNAME(nsgnum, strname, nchk)`**: Retrieves the H-M symbol into `strname (CHARACTER(len=sg_soplen), INTENT(OUT))`.
    - **`SG_NUMGETPATN(nsgnum, npatn, nchk)`**: Retrieves the Patterson space group number into `npatn (INTEGER, INTENT(OUT))`.
    - **`SG_NUMGETSYMNUM(nsgnum, nsymnum, nchk)`**: Retrieves the count of symmetry operations into `nsymnum (INTEGER, INTENT(OUT))`.
    - **`SG_NUMGETSYMOP(nsgnum, isymop, strsymop, nchk)`**: Retrieves the `isymop`-th symmetry operation string into `strsymop (CHARACTER(len=sg_soplen), INTENT(OUT))`.
    - **`SG_NUMGETSYMOPS(nsgnum, strsymops, nsymnum, nchk)`**: Retrieves all symmetry operation strings for `nsgnum` into the allocatable array `strsymops (CHARACTER(len=sg_soplen), DIMENSION(:), ALLOCATABLE, INTENT(OUT))`. `nsymnum` returns the count.

- **Data Access Routines (by Space Group H-M Symbol `strname`)**:
    These routines first call `SG_NAMGETNUM` to find the corresponding space group number, then use the "by number" routines.
    - **`SG_NAMGETNUM(strname, nsgnum)`**: Retrieves the space group number `nsgnum (INTEGER, INTENT(OUT))` for a given `strname (CHARACTER(len=*), INTENT(IN))`. `nsgnum` is 0 if the name is not found.
    - **`SG_NAMGETPATN(strname, npatn, nchk)`**
    - **`SG_NAMGETSYMNUM(strname, nsymnum, nchk)`**
    - **`SG_NAMGETSYMOP(strname, isymop, strsymop, nchk)`**
    - **`SG_NAMGETSYMOPS(strname, strsymops, nsymnum, nchk)`**

## Important Variables/Constants

- **`sg_nummax (INTEGER, PARAMETER)`**: Defines the total number of space groups (230).
- **`sg_soplen (INTEGER, PARAMETER)`**: Defines the string length for symmetry operation representations (32 characters).
- **Error code `nchk`**: Returned by most getter routines to indicate success (1) or specific errors (e.g., -1 for initialization failure, -2 for invalid space group number/name, -3 for invalid symmetry operation index).

## Usage Examples

```fortran
USE spacegroups
IMPLICIT NONE

CHARACTER(len=sg_soplen) :: hm_symbol, single_sym_op
CHARACTER(len=sg_soplen), DIMENSION(:), ALLOCATABLE :: all_sym_ops
INTEGER :: sg_number, patterson_sg_num, num_ops, status_check, i

! Explicitly initialize (optional, getters will auto-initialize if needed)
CALL SG_INIT(status_check)
IF (status_check /= 1) THEN
  PRINT *, "Error: Spacegroup module initialization failed."
  STOP
END IF

! Get information for space group number 225 (Fm-3m)
sg_number = 225
CALL SG_NUMGETNAME(sg_number, hm_symbol, status_check)
IF (status_check == 1) THEN
  PRINT *, "Space Group #", sg_number, " is ", TRIM(hm_symbol)
END IF

CALL SG_NUMGETSYMNUM(sg_number, num_ops, status_check)
IF (status_check == 1) THEN
  PRINT *, "It has ", num_ops, " symmetry operations."
END IF

CALL SG_NUMGETSYMOP(sg_number, 1, single_sym_op, status_check) ! Get the first operation
IF (status_check == 1) THEN
  PRINT *, "The first symmetry operation is: ", TRIM(single_sym_op)
END IF

! Get all symmetry operations for space group "P4/mmm" (Number 123)
CALL SG_NAMGETSYMOPS("P4/mmm", all_sym_ops, num_ops, status_check)
IF (status_check == 1 .AND. ALLOCATED(all_sym_ops)) THEN
  PRINT *, "Space group P4/mmm (", num_ops, " ops):"
  DO i = 1, num_ops
    PRINT *, "  ", TRIM(all_sym_ops(i))
  END DO
  DEALLOCATE(all_sym_ops)
END IF

! De-initialize when done (optional, but good practice for library use)
CALL SG_UNINIT()
CALL SG_ISREADY(status_check)
PRINT *, "Module initialized after UNINIT: ", status_check ! Expected: 0

END PROGRAM example_spacegroups
```

## Dependencies and Interactions

- **`comv`**: The `USE comv` statement is present, likely for the `dp` kind parameter or other global settings, though not explicitly used by the public interface routines shown.
- **`constants`**: The `USE constants` statement is present, but no constants from this module are directly used by the public interface routines.
- **`messages`**: The `USE messages` statement is present, but `ATOMSK_MSG` is not used by the public getter routines (they use `nchk` for error status). The extensive `SG_INIT` subroutine (not fully detailed in the provided source snippet, but whose data section is very long) contains the actual space group data.
- **Data Source**: The symmetry operation data is noted to be taken from `cci.lbl.gov/cctbx/explore_symmetry.html` and edited by Dr. J. Barthel.
- **Memory Management**: The module uses allocatable arrays for storing space group data. `SG_INIT` allocates approximately 1.4MB of heap memory. `SG_UNINIT` must be called to deallocate this memory if fine-grained control is needed, otherwise it persists for the program's lifetime after first use.
- **Compilation Note**: A comment highlights a historical compilation issue with gfortran 4.4 and the `-ftree-pre` optimization flag (part of `-O2` and higher), which could cause excessive RAM usage or compiler errors due to the large data initialization section. The recommendation was to compile with `-O1` or `-fno-tree-pre` if using such a compiler version.
