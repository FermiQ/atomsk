# `readin.f90`

## Overview

The `readin.f90` module serves as the primary entry point for reading atomic structure data from various file formats into Atomsk. Its main purpose is to identify the format of a given input file and then dispatch the reading task to the appropriate format-specific subroutine (located in separate `in_*.f90` modules). After the format-specific reader has processed the file, this module retrieves the cell vectors, atomic coordinates, atomic numbers/types, optional shell information, comments, and any auxiliary atomic properties. If cell vectors are not explicitly provided in the input file, this module attempts to determine them. It also performs consistency checks on the dimensions of the returned arrays.

## Key Components

- **`MODULE readin`**: The main module that orchestrates the file reading process.

- **`SUBROUTINE READ_AFF(inputfile, H, P, S, comment, AUXNAMES, AUX)`**:
    - This is the core public subroutine for reading an atomic data file.
    - **Inputs**:
        - `inputfile (CHARACTER(LEN=*), INTENT(IN))`: The name (path) of the file to be read.
    - **Outputs**:
        - `H (REAL(dp), DIMENSION(3,3), INTENT(OUT))`: The 3x3 matrix storing the cell basis vectors. H(1,:), H(2,:), H(3,:) are the three vectors.
        - `P (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT))`: A 2D allocatable array. After successful execution, it contains the primary atomic data. Each row `P(i,:)` typically stores `[x, y, z, type/atomic_number]` for atom `i`.
        - `S (REAL(dp), DIMENSION(:,:), ALLOCATABLE, OPTIONAL, INTENT(OUT))`: An optional 2D allocatable array for core-shell models. If present and shells are read, it has the same row dimension as `P` and stores shell coordinates and types.
        - `comment (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, OPTIONAL, INTENT(OUT))`: An optional allocatable array of strings to store any comments or header information read from the input file.
        - `AUXNAMES (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, OPTIONAL, INTENT(OUT))`: An optional allocatable array of strings storing the names of auxiliary properties (e.g., "vx", "vy", "vz", "fx", "charge").
        - `AUX (REAL(dp), DIMENSION(:,:), ALLOCATABLE, OPTIONAL, INTENT(OUT))`: An optional 2D allocatable array storing the values of auxiliary properties for each atom. `AUX(i,j)` would be the j-th auxiliary property for the i-th atom.
    - **Logic**:
        1.  Initializes output arrays (deallocates if already allocated, sets H to zero).
        2.  Calls `CHECKFILE` (from `files` module) to verify the existence and readability of `inputfile`.
        3.  Calls `GUESS_FORMAT` (from `guess_form` module) to determine the `infileformat` (a 5-character string code, e.g., "xsf  ", "cfg  ") by inspecting the file's extension and/or content.
        4.  If `infileformat` is "xxx" (unknown/error), it sets `nerr` and exits.
        5.  Uses a `SELECT CASE(infileformat)` block to call the appropriate format-specific reading subroutine (e.g., `CALL READ_XSF(...)`, `CALL READ_CFG(...)`, etc.). These subroutines are `USE`d from their respective `in_*.f90` modules.
        6.  If `nerr` (global error flag from `comv`) is set by a reader, `READ_AFF` exits.
        7.  If cell vectors `H` are zero (i.e., not found or set by the format reader), it calls `DETERMINE_H` (from `deterH` module) to attempt to deduce them from atomic coordinates.
        8.  Calls `CHECK_ARRAY_CONSISTENCY` (from `subroutines` module) to ensure that `P`, `S`, `AUX`, and `AUXNAMES` have consistent dimensions if they are allocated.
        9.  Provides verbose output of array sizes and cell vectors if `verbosity` (global, from `comv`) is high.
        10. Reports the total number of atoms (and shells, if present) read.

## Important Variables/Constants

- **`inputfile (CHARACTER(LEN=*), INTENT(IN))`**: The path to the file to be read.
- **`H (REAL(dp), DIMENSION(3,3), INTENT(OUT))`**: Stores the read or determined cell vectors.
- **`P (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT))`**: Stores atomic coordinates and types.
- **Optional Output Arrays**: `S`, `comment`, `AUXNAMES`, `AUX` store shells, comments, auxiliary property names, and auxiliary property values, respectively. Their allocation and content depend on the input file format and content.

## Usage Examples

The `READ_AFF` subroutine is a cornerstone of Atomsk's file input mechanism, typically called near the beginning of most operations that involve an input atomic structure file.

```fortran
MODULE MainProgram
  USE readin
  USE comv ! For dp, nerr, verbosity
  IMPLICIT NONE

  CHARACTER(LEN=256) :: filename_to_read
  REAL(dp), DIMENSION(3,3) :: cell_matrix
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: atom_data, shell_data, aux_data
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: comments_read, aux_names_read

  filename_to_read = "input.xsf"
  verbosity = 1 ! Set global verbosity

  CALL READ_AFF(filename_to_read, cell_matrix, atom_data, &
                S_shells=shell_data, comment=comments_read, &
                AUXNAMES=aux_names_read, AUX=aux_data)

  IF (nerr > 0) THEN
    PRINT *, "Error reading file: ", TRIM(filename_to_read)
    STOP
  END IF

  IF (ALLOCATED(atom_data)) THEN
    PRINT *, "Read ", SIZE(atom_data,1), " atoms."
    PRINT *, "Cell vectors:"
    PRINT *, cell_matrix(1,:)
    PRINT *, cell_matrix(2,:)
    PRINT *, cell_matrix(3,:)
    ! ... further processing ...
    DEALLOCATE(atom_data)
    IF (ALLOCATED(shell_data)) DEALLOCATE(shell_data)
    IF (ALLOCATED(comments_read)) DEALLOCATE(comments_read)
    IF (ALLOCATED(aux_names_read)) DEALLOCATE(aux_names_read)
    IF (ALLOCATED(aux_data)) DEALLOCATE(aux_data)
  ELSE
    PRINT *, "No atom data was read from file: ", TRIM(filename_to_read)
  END IF

END MODULE MainProgram
```

## Dependencies and Interactions

- **`comv`**: Provides global variables like `dp` (double precision kind), `verbosity` level, and `nerr` (error counter).
- **`constants`**: Included via `USE constants`.
- **`messages`**: Uses `ATOMSK_MSG` for console output.
- **`files`**: Uses `CHECKFILE` to verify file existence and `FILE_SIZE` for informational messages.
- **`files_msg`**: Included via `USE files_msg`, likely for file-related message strings or codes.
- **`subroutines`**: Uses `CHECK_ARRAY_CONSISTENCY` and `IS_INTEGER`.
- **`guess_form`**: Critically depends on `GUESS_FORMAT` to determine the input file type.
- **`deterH`**: Uses `DETERMINE_H` to calculate cell vectors if they are not present in the input file.
- **Format-Specific `in_*` Modules**: This module acts as a dispatcher to a large number of specialized modules, each designed to read a particular file format (e.g., `in_xsf` for XSF files, `in_cfg` for CFG files, `in_vasp_poscar` for VASP POSCAR files, etc.). The `USE` statements at the beginning of `readin.f90` list all such supported input modules.
- **Error Handling**: If `GUESS_FORMAT` cannot determine the file type, or if a format-specific reader encounters an error (which typically sets the global `nerr`), `READ_AFF` will terminate its process and `nerr` will indicate failure to the calling routine.
