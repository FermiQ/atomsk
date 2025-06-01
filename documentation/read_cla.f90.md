# `read_cla.f90`

## Overview

The `read_cla.f90` module is a critical component of Atomsk responsible for parsing and interpreting command-line arguments (CLAs). When a user runs Atomsk, they provide various arguments that specify the desired operation (mode), input files, output file formats, and a wide range of options to manipulate atomic structures or control the program's behavior. This module takes the raw array of command-line strings and processes them into structured information that the main Atomsk program can then act upon. It identifies the primary mode of operation, extracts filenames, populates an array with recognized options and their specific parameters, and lists the desired output file formats.

## Key Components

- **`MODULE read_cla`**: The main module containing the command-line argument parsing logic.

- **`SUBROUTINE GET_CLA(cla, mode, options_array, outfileformats, pfiles, mode_param)`**:
    - This is the central subroutine of the module. It iterates through the input array of command-line arguments (`cla`) and categorizes each argument.
    - **Inputs**:
        - `cla (CHARACTER(LEN=4096), DIMENSION(:), ALLOCATABLE, INTENT(IN))`: An array where each element is a command-line argument string.
        - `options_array (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`: Passed in (potentially allocated but empty) to be filled with recognized options and their parameters.
    - **Outputs**:
        - `mode (CHARACTER(LEN=12), INTENT(OUT))`: Stores the primary operational mode determined from the arguments (e.g., "--create", "--diff", "gather", "unwrap", "normal" for simple conversion, etc.).
        - `options_array`: Filled with strings, where each string represents an option and its parsed parameters (e.g., "-select above 0.5 x", "-deform x 0.1 0.3").
        - `outfileformats (CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE, INTENT(OUT))`: An array populated with the 5-character codes for requested output file formats (e.g., "xsf  ", "cfg  ", "lmp  ").
        - `pfiles (CHARACTER(LEN=4096), DIMENSION(5), INTENT(OUT))`: An array to store primary filenames.
            - `pfiles(1)`: Typically the main input file.
            - `pfiles(2)`: Typically the main output file (if not specified by format keywords directly).
            - `pfiles(3)`: Secondary input file (e.g., for diff, cpprop, unwrap modes).
            - `pfiles(4)`: Secondary input/output file (e.g., for diff, cpprop, unwrap modes).
            - `pfiles(5)`: File containing a list of other files (e.g., for --list, --average, --rdf modes).
        - `mode_param (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(OUT))`: An array to store specific parameters required by certain modes. For example:
            - For `--create`: structure type, lattice parameters, atomic species, orientation.
            - For `--merge`: stacking/rescale parameters, number of files, list of files.
            - For `--rdf`: R_max and dR.
            - For `--interpolate`: number of images.
    - **Logic**: The subroutine employs a large `DO WHILE` loop to iterate through the `cla` array. Inside, a series of `IF/ELSEIF` statements check each argument:
        - Arguments starting with `"--"` are typically interpreted as operational modes.
        - Arguments starting with `"-"` (but not `"--"`) are interpreted as options (e.g., `-select`, `-cut`, `-deform`). The subroutine then attempts to read the required number of subsequent arguments as parameters for that option.
        - Arguments matching known file format keywords (e.g., "xsf", "cfg", "vasp", "lammps") are added to the `outfileformats` list.
        - Other arguments are usually treated as filenames and are assigned to `pfiles(1)` or `pfiles(2)` etc., based on the context.
        - The subroutine includes error checking for incorrect option syntax or missing parameters, setting `nerr` (a global error counter from the `comv` module) and calling `ATOMSK_MSG` or `DISPLAY_HELP` for user feedback.

## Important Variables/Constants

- **`cla (CHARACTER(LEN=4096), DIMENSION(:), ALLOCATABLE, INTENT(IN))`**: The input array holding all command-line arguments as strings.
- **`mode (CHARACTER(LEN=12), INTENT(OUT))`**: The determined primary mode of operation for Atomsk.
- **`options_array (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`**: Stores each recognized option along with its specific parameters as a single string. For example, `"-select above 0.5 x"`.
- **`outfileformats (CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE, INTENT(OUT))`**: A list of 5-character strings representing the file formats for output (e.g., "xsf  ", "lmp  ").
- **`pfiles (CHARACTER(LEN=4096), DIMENSION(5), INTENT(OUT))`**: Stores the primary filenames identified in the command line.
- **`mode_param (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(OUT))`**: Stores additional parameters specific to certain operational modes.

## Usage Examples

The `GET_CLA` subroutine is called internally by the main Atomsk program. Users interact with it indirectly by providing command-line arguments when running Atomsk.

**Conceptual call within Atomsk:**
```fortran
PROGRAM ATOMSK_MAIN
  USE read_cla
  USE comv
  IMPLICIT NONE
  CHARACTER(LEN=4096), DIMENSION(:), ALLOCATABLE :: cmd_args_list
  CHARACTER(LEN=12) :: current_mode
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: program_options
  CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE :: output_formats_list
  CHARACTER(LEN=4096), DIMENSION(5) :: program_files
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: mode_specific_params
  INTEGER :: num_args, i

  ! 1. Get command line arguments into cmd_args_list
  ! (Example: using Fortran intrinsics GET_COMMAND_ARGUMENT)
  num_args = COMMAND_ARGUMENT_COUNT()
  IF (num_args > 0) THEN
    ALLOCATE(cmd_args_list(num_args))
    DO i = 1, num_args
      CALL GET_COMMAND_ARGUMENT(i, cmd_args_list(i))
    END DO
  END IF

  ! 2. Allocate options_array (size can be estimated or fixed maximum)
  IF (num_args > 0) ALLOCATE(program_options(num_args)) ! Max possible options

  ! 3. Call GET_CLA to parse
  CALL GET_CLA(cmd_args_list, current_mode, program_options, &
               & output_formats_list, program_files, mode_specific_params)

  ! 4. Check nerr from comv module for parsing errors
  IF (nerr > 0) THEN
    PRINT *, "Error parsing command line arguments."
    STOP
  END IF

  ! 5. Proceed with Atomsk operations based on parsed values
  ! PRINT *, "Mode:", current_mode
  ! PRINT *, "Input File:", program_files(1)
  ! IF (ALLOCATED(program_options)) THEN
  !   PRINT *, "Options:", program_options
  ! END IF
  ! ... and so on

END PROGRAM ATOMSK_MAIN
```

**Example Atomsk command lines parsed by `GET_CLA`:**
- `atomsk system.cfg -duplicate 2 2 1 final.xsf lmp`
- `atomsk --create fcc 3.6 Al final.cfg`
- `atomsk initial.xyz -select above 0.5 z -remove-atoms selected final.xyz`
- `atomsk --polycrystal unitcell.dat voronoi_seeds.txt polycrystal.xsf`

## Dependencies and Interactions

- **`atoms`**: Uses `ATOMNUMBER` to validate atomic species provided in modes like `--create`.
- **`comv`**: Heavily relies on global variables from `comv`, including:
    - `nerr`, `nwarn`: For error and warning counting.
    - `verbosity`: To control debug message output.
    - `flist`: An array of known file extensions/formats, used to identify format keywords.
    - `ofu`: Output file unit (though not directly used for parsing, its value might be set based on CLA like "-").
- **`strings`**: Uses `StrDnCase` and `StrUpCase` for case-insensitive comparison of arguments.
- **`constants`**: Included with `USE constants`, likely for `dp` and other fundamental constants, though direct usage in `GET_CLA` is minimal.
- **`messages`**: Uses `ATOMSK_MSG` for displaying messages and `DISPLAY_HELP` for showing help sections upon error or specific requests.
- **`files`**: Included with `USE files`, but its subroutines are not directly called within `GET_CLA`.
- **`subroutines`**: Included with `USE subroutines`, but its subroutines are not directly called within `GET_CLA`.
- **Error Handling**: If invalid arguments or missing parameters for options/modes are detected, `GET_CLA` increments `nerr` and often calls `ATOMSK_MSG` and/or `DISPLAY_HELP` to inform the user. The main program is expected to check `nerr` after calling `GET_CLA`.
