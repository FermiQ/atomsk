# `writeout.f90`

## Overview

The `writeout.f90` module serves as the central dispatcher for writing atomic structure data from Atomsk's internal representation to various output file formats. Its primary subroutine, `WRITE_AFF`, takes the complete description of an atomic system (including cell vectors, atomic positions and types, optional core-shell information, comments, and auxiliary properties) along with a base filename (`prefix`) and a list of desired output formats. For each format specified, it constructs the appropriate output filename and calls a dedicated format-specific subroutine (from an `out_*.f90` module) to perform the actual writing. The module also includes pre-writing checks, such as for NaN values, data consistency, and potential issues if a chosen format does not support all features of the atomic data (e.g., shells, partial occupancies).

## Key Components

- **`MODULE writeout`**: The main module that orchestrates the file writing process.

- **`SUBROUTINE WRITE_AFF(prefix, outfileformats, H, P, S, comment, AUXNAMES, AUX)`**:
    - This is the core public subroutine for writing atomic data to one or more output files.
    - **Inputs**:
        - `prefix (CHARACTER(LEN=*), INTENT(IN))`: The base name for the output file(s). The actual filenames will be `prefix.ext`.
        - `outfileformats (CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`: An allocatable array of 5-character strings defining the target file formats (e.g., "xsf  ", "cfg  "). This array can be modified by the routine (e.g., if `prefix` itself contains a recognized extension).
        - `H (REAL(dp), DIMENSION(3,3), INTENT(IN))`: The 3x3 matrix of supercell basis vectors.
        - `P (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(IN))`: The primary atomic data array, where `P(i,1:3)` are coordinates and `P(i,4)` is the atomic type/number.
        - `S (REAL(dp), DIMENSION(:,:), ALLOCATABLE, OPTIONAL, INTENT(IN))`: Optional array for core-shell model data.
        - `comment (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, OPTIONAL, INTENT(INOUT))`: Optional array of strings for comments to be included in output files. Can be generated if not provided.
        - `AUXNAMES (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, OPTIONAL, INTENT(IN))`: Optional array of names for auxiliary properties.
        - `AUX (REAL(dp), DIMENSION(:,:), ALLOCATABLE, OPTIONAL, INTENT(IN))`: Optional array of auxiliary property values for each atom.
    - **Logic**:
        1.  **Initial Checks**: Handles `prefix == "NULL"` case (skip writing). Reports number of atoms and shells. Checks if `P` is allocated.
        2.  **NaN Check**: Calls `CHECKNAN` to scan `P` (up to 10,000 atoms), `H`, and `AUX` for NaN/Infinity values. Sets `nerr` on failure.
        3.  **Array Consistency**: Calls `CHECK_ARRAY_CONSISTENCY` to validate dimensions of `P`, `S`, `AUX`, `AUXNAMES`.
        4.  **Comment Generation**: If `comment` is not provided or is a default Atomsk generation message, it creates a new comment with current date, time, and username. Ensures all comment lines start with '#'.
        5.  **Output Filename/Format Handling**:
            - If `prefix` is empty and not writing to stdout (`ofu /= 6`), prompts user for a prefix.
            - If `prefix` contains a known extension, that format is added to `outfileformats` via `SET_OUTPUT`.
            - If `outfileformats` is empty, prompts user for a format.
            - Removes duplicate format requests from `outfileformats`.
        6.  **Data Feature Warnings**:
            - Calculates total charge if "q" (charge) or "qs" (shell charge) auxiliary properties exist and warns if non-zero.
            - If atom "type" auxiliary property is used, checks for conflicts (different species with same type) and warns if output formats use types.
            - Warns if core-shell data (`S` array) is present but an output format does not support shells.
            - Warns if partial occupancies ("occ" auxiliary property) are present and differ from 1, but an output format does not support them.
        7.  **Main Writing Loop**: Iterates through each format string in `outfileformats`.
            - Constructs `outputfile` name using `NAME_OUTFILE` (from `files` module).
            - Checks for file existence based on `ignore` and `overw` flags (global, from `comv`).
            - Calls the appropriate format-specific writer subroutine (e.g., `CALL WRITE_XSF(...)`, `CALL WRITE_CFG(...)`) from the various `out_*.f90` modules.
            - Reports success or errors for each file written.

## Important Variables/Constants

- **`prefix (CHARACTER(LEN=*), INTENT(IN))`**: Base for output filenames.
- **`outfileformats (CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`**: List of target output formats.
- **Input data arrays**: `H`, `P`, and optional `S`, `comment`, `AUXNAMES`, `AUX` define the atomic system to be written.
- **Global variables from `comv`**: `ofu` (output file unit), `overw` (overwrite flag), `ignore` (ignore existing file flag), `verbosity`, `nwarn`, `nerr` are used to control behavior and report status.

## Usage Examples

`WRITE_AFF` is typically the final step in many Atomsk operations, called after data is loaded and processed.

```fortran
MODULE MainAtomskProgram
  USE writeout
  USE comv ! For dp, nerr, verbosity, etc.
  IMPLICIT NONE

  CHARACTER(LEN=256) :: output_prefix
  CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE :: desired_formats
  REAL(dp), DIMENSION(3,3) :: cell_H_final
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: atoms_P_final, shells_S_final, aux_data_final
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: comments_final, aux_names_final

  ! ... (Atomsk operations: read files, apply options, create structures, etc.) ...
  ! ... (Populate cell_H_final, atoms_P_final, and optional arrays) ...

  output_prefix = "final_structure"
  IF (ALLOCATED(desired_formats)) DEALLOCATE(desired_formats)
  ALLOCATE(desired_formats(2))
  desired_formats(1) = "xsf  "
  desired_formats(2) = "cfg  "

  ! Assuming comments_final, shells_S_final, aux_names_final, aux_data_final are
  ! either properly allocated with data or not allocated if not needed.

  CALL WRITE_AFF(output_prefix, desired_formats, cell_H_final, atoms_P_final, &
                 S=shells_S_final, comment=comments_final, &
                 AUXNAMES=aux_names_final, AUX=aux_data_final)

  IF (nerr > 0) THEN
    PRINT *, "Error(s) occurred during file writing."
  ELSE
    PRINT *, "Successfully wrote output files."
  END IF

  ! ... (Cleanup) ...
  IF (ALLOCATED(desired_formats)) DEALLOCATE(desired_formats)
  IF (ALLOCATED(atoms_P_final)) DEALLOCATE(atoms_P_final)
  ! ... deallocate other optional arrays if they were allocated ...

END MODULE MainAtomskProgram
```

## Dependencies and Interactions

- **Core Utility Modules**:
    - `comv`: For global variables (`dp`, `ofu`, `overw`, `ignore`, `verbosity`, `nwarn`, `nerr`).
    - `constants`: Included via `USE constants`.
    - `atoms`: `USE atoms` is present.
    - `guess_form`: `USE guess_form` is present (though `GUESS_FORMAT` is not directly called in `WRITE_AFF` but in `NAME_OUTFILE` from `files`).
    - `messages`: For `ATOMSK_MSG`.
    - `files`: Uses `NAME_OUTFILE`, `CHECKFILE`, `FILE_SIZE`.
    - `files_msg`: Included via `USE files_msg`.
    - `strings`: `USE strings` is present.
    - `subroutines`: Uses `CHECKNAN`, `CHECK_ARRAY_CONSISTENCY`, `FIND_NSP`, `CREATE_DATE`, `IS_INTEGER`.
- **Format-Specific `out_*` Modules**: This module is a central dispatcher to numerous writer modules, each responsible for a specific file format (e.g., `out_atsk`, `out_cfg`, `out_xsf`, `out_vasp_poscar`, etc.). The `USE` statements list all supported output modules.
- **Error Handling**: The subroutine checks for various potential issues (NaN values, inconsistent arrays, unsupported features by format) and reports them by incrementing `nwarn` or `nerr` and calling `ATOMSK_MSG`.
- **File System**: Interacts heavily with the file system by creating and writing to multiple output files.
