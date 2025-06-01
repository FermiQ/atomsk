# `options.f90`

## Overview

The `options.f90` module is a central processing unit within Atomsk that applies a series of user-specified transformations and manipulations (options) to an atomic system. It takes the current state of the system (cell vectors, atomic positions, shell positions, auxiliary properties, crystal orientation, selection mask, and elastic tensor) and an array of option strings. Each option string, parsed from the command line by `read_cla.f90`, dictates a specific operation. The `OPTIONS_AFF` subroutine iterates through these options and calls dedicated subroutines (typically from `opt_*.f90` modules) to execute each one, sequentially modifying the atomic system.

## Key Components

- **`MODULE options`**: The main module definition.

- **`SUBROUTINE OPTIONS_AFF(options_array, Huc, H, P, S, AUXNAMES, AUX, ORIENT, SELECT, C_tensor)`**:
    - This is the core subroutine that iterates through the `options_array` and applies each option to the atomic system.
    - **Inputs**:
        - `options_array (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(IN))`: An array of strings, where each string contains an option keyword (e.g., "-cut", "-deform") and its associated parameters.
        - `Huc (REAL(dp), DIMENSION(3,3), INTENT(INOUT))`: Basis vectors of the unit cell. Some options might use or modify this.
        - `ORIENT (REAL(dp), DIMENSION(3,3), INTENT(INOUT))`: Crystallographic orientation matrix of the system. Modified by options like `-orient`.
        - `C_tensor (REAL(dp), DIMENSION(9,9), INTENT(INOUT))`: 9x9 elastic tensor. Modified by options like `-stress` or `-orient`.
    - **Inputs/Outputs (State of the atomic system, modified by options)**:
        - `H (REAL(dp), DIMENSION(3,3), INTENT(INOUT))`: Supercell basis vectors.
        - `P (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT))`: Atomic positions (x,y,z) and types/atomic numbers (column 4). Can be reallocated if atoms are added/removed.
        - `S (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT))`: Shell positions and types (for core-shell models). Can be reallocated.
        - `AUXNAMES (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`: Names of auxiliary atomic properties.
        - `AUX (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT))`: Values of auxiliary properties for each atom.
        - `SELECT (LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`: A logical mask indicating selected atoms. Modified by options like `-select`, `-remove-atom`.
    - **Logic**:
        1.  Initializes `ORIENT` to a default identity matrix if it's zero.
        2.  Performs a preliminary check on the order of options, issuing warnings for potentially problematic sequences (e.g., `-duplicate` before `-wrap`).
        3.  Iterates through each string in `options_array`.
        4.  For each option string, it extracts the `optionname` (e.g., "-add-atom", "-cut", "-rotate").
        5.  A large `SELECT CASE(optionname)` block then:
            - Parses the specific parameters for that option from the `options_array(ioptions)` string. This often involves reading numbers, keywords, or filenames.
            - For parameters that can be relative to box dimensions (e.g., "0.5*box", "BOX-1.0"), it calls `BOX2DBLE` (from the `strings` module) for evaluation.
            - Calls the corresponding specialized subroutine (e.g., `ADDATOM_XYZ`, `CUTCELL`, `ROTATE_XYZ`) to perform the action. These subroutines are imported from various other `opt_*.f90` modules.
        6.  After each option is applied, it performs several checks:
            - If `P` (atoms) becomes unallocated or empty, it issues a warning.
            - Ensures consistency in sizes of `S` (shells) and `SELECT` with `P`.
            - If `C_tensor` has been modified, it checks for NaN values and symmetry.
        7.  If `verbosity` (global, from `comv`) is set to 4 (debug), it writes extensive information about the state of the system after each option, including atomic positions to "atomsk.xyz" (or "atomsk_readin.adbg" in older versions) and selected atoms to "atomsk_select.xyz".
        8.  If `nerr` (global error flag from `comv`) is set at any point, the loop over options is exited.
        9.  After all options are processed, a final consistency check on arrays is performed.

## Important Variables/Constants

- The input/output arguments of `OPTIONS_AFF` are the most crucial, as they represent the evolving state of the atomic system as options are applied sequentially.
- The module itself does not define public constants but relies on those from `comv` and `constants`.

## Usage Examples

The `OPTIONS_AFF` subroutine is called by Atomsk's main program or by mode-specific handlers after an initial atomic structure has been read (e.g., by `READ_AFF`) and the command-line options have been parsed into `options_array` (by `GET_CLA`).

```fortran
! Conceptual call within Atomsk, after READ_AFF and GET_CLA
PROGRAM ATOMSK_WORKFLOW
  USE options
  USE comv
  ! ... other necessary modules ...
  IMPLICIT NONE

  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: parsed_options_list
  REAL(dp), DIMENSION(3,3) :: unit_cell_H, super_cell_H, orientation_mat
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: atoms_P, shells_S, aux_props_AUX
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: aux_prop_names
  LOGICAL, DIMENSION(:), ALLOCATABLE :: selected_atoms
  REAL(dp), DIMENSION(9,9) :: elastic_C_tensor

  ! ... (1. Read initial system into H, P, S, AUXNAMES, AUX, ORIENT, C_tensor using READ_AFF)
  ! ... (2. Parse command-line into parsed_options_list using GET_CLA)

  IF (nerr == 0 .AND. ALLOCATED(parsed_options_list)) THEN
    CALL OPTIONS_AFF(parsed_options_list, unit_cell_H, super_cell_H, atoms_P, &
                     & shells_S, aux_prop_names, aux_props_AUX, &
                     & orientation_mat, selected_atoms, elastic_C_tensor)
  END IF

  IF (nerr > 0) THEN
    PRINT *, "An error occurred while applying options."
  ELSE
    ! ... (3. Proceed with output or further mode-specific actions) ...
  END IF

  ! ... (Cleanup allocated arrays) ...
END PROGRAM ATOMSK_WORKFLOW
```

## Dependencies and Interactions

- **Core Utility Modules**:
    - `comv`: For global variables (`dp`, `verbosity`, `nerr`, `nwarn`, etc.).
    - `constants`: For mathematical or physical constants.
    - `strings`: Uses `BOX2DBLE` for parsing dimension-relative parameters.
    - `crystallography`, `messages`, `files`, `subroutines`, `guess_form`, `resize`, `atoms`.
- **Option-Specific Modules**: This module `USE`s a large number of other modules, each dedicated to implementing the logic for one or more command-line options. Examples include:
    - `addatom`, `addshells`, `alignx`, `bindshells`, `carttofrac`, `center`, `cell`, `crack`, `cut_cell`, `deform`, `dislocation`, `disturb`, `duplicate`, `freeze`, `mirror`, `orient`, `orthocell`, `properties`, `reduce_cell`, `remdoubles`, `rmatom`, `rmprop`, `rmshells`, `roll`, `rotate`, `roundoff`, `select`, `separate`, `shift`, `sort`, `spacegroup`, `stress`, `substitute`, `swap`, `torsion`, `unit`, `unskew`, `velocity`, `wrap`.
- **Sequential Execution**: The order of options in the `options_array` is critical, as they are applied sequentially. The state of the atomic system after one option becomes the input for the next.
- **Array Reallocation**: Many options can change the number of atoms (e.g., `-add-atom`, `-remove-atom`, `-duplicate`, `-cut`). The `P`, `S`, `AUX`, and `SELECT` arrays are dynamically reallocated by the respective option subroutines when this occurs.
- **Error Handling**: If an option subroutine encounters an error, it typically sets the global `nerr` flag. `OPTIONS_AFF` checks `nerr` after each option call and will terminate the loop if an error is flagged.
