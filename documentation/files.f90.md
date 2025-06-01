# `files.f90`

## Overview

The `files.f90` module provides a collection of utility subroutines and functions designed to handle common file-related operations within the Atomsk software. These utilities include dynamically finding an unused Fortran I/O unit number for opening new files, managing a list of specified output file formats, intelligently constructing output filenames based on input filenames and desired new formats, and obtaining a human-readable string representing a file's size.

## Key Components

- **`MODULE files`**: The main Fortran module encapsulating these file utility routines.

- **`FUNCTION FREEUNIT(imin, imax) RESULT(iunit)`**:
    - **Inputs (Optional)**:
        - `imin (INTEGER)`: The minimum unit number to check (default is 11).
        - `imax (INTEGER)`: The maximum unit number to check (default is 99).
    - **Output**: `iunit (INTEGER)` - An available (not currently open) Fortran I/O unit number within the specified range. Returns 0 if no unit is found (highly unlikely in normal usage).
    - **Description**: Iterates through unit numbers from `istart` to `iend`, using `INQUIRE` to check if a unit is already open. Returns the first unopened unit number it finds.

- **`SUBROUTINE SET_OUTPUT(outfileformats, extension, l_value)`**:
    - **Inputs**:
        - `outfileformats (CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`: An allocatable array of 5-character strings representing the list of desired output file formats (e.g., "xsf  ", "cfg  ").
        - `extension (CHARACTER(LEN=5), INTENT(IN))`: The file extension to add or remove.
        - `l_value (LOGICAL, INTENT(IN))`: A logical flag. If `.TRUE.`, `extension` is added to `outfileformats`. If `.FALSE.`, `extension` is removed. If `extension` is "all  " and `l_value` is `.FALSE.`, all formats are removed (array is deallocated).
    - **Description**: Dynamically manages the `outfileformats` array. Adding involves reallocating with one more element. Removing involves finding the extension and reallocating with one less element, or deallocating if "all" is specified or it's the last one.

- **`SUBROUTINE NAME_OUTFILE(inputfile, outputfile, outfileformat)`**:
    - **Inputs**:
        - `inputfile (CHARACTER(LEN=*), INTENT(IN))`: The name of the input file.
        - `outfileformat (CHARACTER(LEN=5), INTENT(IN))`: The desired 5-character extension for the output file (e.g., "cfg  ").
    - **Output**: `outputfile (CHARACTER(LEN=*), INTENT(OUT))`: The generated name for the output file.
    - **Description**: Constructs an output filename. It typically takes the `inputfile` name, removes its existing extension if it's a known Atomsk format (checked against `flist` from the `comv` module), and then appends the new `outfileformat`. For example, `input.xyz` with `outfileformat="cfg  "` might become `input.cfg`. If the global variable `ofu` (from `comv`) is 6 (indicating standard output), `outputfile` is set to an empty string.

- **`FUNCTION FILE_SIZE(filename) RESULT(filesize)`**:
    - **Input**: `filename (CHARACTER(LEN=*), INTENT(IN))`: The name of the file to check.
    - **Output**: `filesize (CHARACTER(LEN=16))`: A string representing the file's size in a human-readable format (e.g., "123  ", "40.2 k", "3.6 G", "1.2 T"). Returns a blank string if the file does not exist.
    - **Description**: Uses `INQUIRE` to get the file size in bytes and then formats it into a string with appropriate units (k, M, G, T).

## Important Variables/Constants

- **`outfileformats (CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE)`**: (In `SET_OUTPUT`) An array that holds the list of currently selected output file format extensions.
- **`ofu (INTEGER)`**: (Used in `NAME_OUTFILE`, from `comv` module) A global variable indicating the primary output file unit. If `ofu == 6`, it implies output is to standard output, affecting `NAME_OUTFILE`'s behavior.
- **`flist (CHARACTER(LEN=5), DIMENSION(:,:))`**: (Used in `NAME_OUTFILE`, from `comv` module) A global array listing known file extensions recognized by Atomsk. This is used to intelligently strip existing extensions before appending a new one.
- **`pathsep (CHARACTER(LEN=1))`**: (Used in `NAME_OUTFILE`, from `comv` module) A global variable specifying the system's path separator character (e.g., '/' for Unix/Linux, '\' for Windows).

## Usage Examples

```fortran
USE files
USE comv ! For dp, and implicitly for ofu, flist, pathsep if used by these routines
IMPLICIT NONE

INTEGER :: unit_num
CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE :: current_formats
LOGICAL :: add_format
CHARACTER(LEN=256) :: input_name, output_name_cfg
CHARACTER(LEN=5) :: new_format
CHARACTER(LEN=16) :: size_display

! Get a free I/O unit
unit_num = FREEUNIT()
PRINT *, "Free unit found:", unit_num
! Example: OPEN(UNIT=unit_num, FILE="my_temp_file.dat", STATUS="NEW")
!          CLOSE(unit_num)

! Manage output file formats
add_format = .TRUE.
CALL SET_OUTPUT(current_formats, "xsf  ", add_format)
CALL SET_OUTPUT(current_formats, "cfg  ", add_format)
PRINT *, "Current formats:", current_formats(1), current_formats(2) ! Expected: xsf cfg

add_format = .FALSE.
CALL SET_OUTPUT(current_formats, "xsf  ", add_format)
IF (ALLOCATED(current_formats)) THEN
    PRINT *, "Formats after removing xsf:", current_formats(1) ! Expected: cfg
END IF

! Construct an output filename
input_name = "my_structure.xyz"
new_format = "cfg  "
CALL NAME_OUTFILE(input_name, output_name_cfg, new_format)
PRINT *, "Generated output name:", TRIM(output_name_cfg) ! Expected: my_structure.cfg

! Get file size (assuming 'atomsk' executable exists or some other file)
! Create a dummy file for testing
OPEN(UNIT=10, FILE="dummy.txt", STATUS="REPLACE")
WRITE(10,*) "Hello"
CLOSE(10)
size_display = FILE_SIZE("dummy.txt")
PRINT *, "Size of 'dummy.txt':", TRIM(size_display)
! On some systems this might be "6 B" or similar if the OS writes a newline.

END PROGRAM example_files
```

## Dependencies and Interactions

- **`comv`**: This module is crucial as `files.f90` uses several global variables defined in `comv`:
    - `ofu`: The output file unit, checked by `NAME_OUTFILE`.
    - `flist`: A list of known file types, used by `NAME_OUTFILE` to correctly strip old extensions.
    - `pathsep`: The system's path separator, used by `NAME_OUTFILE` when parsing filenames.
    - `dp` (double precision kind parameter) is also implicitly used through `USE comv`.
- **`constants`**: This module is included via `USE constants`, but its constants are not directly referenced in the provided code snippets of the `files` module.
- **Fortran Intrinsic Functions**:
    - `INQUIRE`: Used in `FREEUNIT` (to check if a unit is open) and `FILE_SIZE` (to check file existence and get its size in bytes).
    - String manipulation functions: `TRIM`, `ADJUSTL`, `SCAN`, `LEN_TRIM` are used extensively, especially in `NAME_OUTFILE`.
    - Mathematical functions: `CEILING`, `NINT`, `DBLE` (though `DBLE` is not explicitly used, `REAL(dp)` implies double precision).
- **File System**: `NAME_OUTFILE` and `FILE_SIZE` interact with the file system by parsing filenames and querying file properties. `FREEUNIT` interacts with the Fortran runtime's I/O unit management.
