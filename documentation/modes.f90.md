# `modes.f90`

## Overview

The `modes.f90` module serves as the central dispatcher for the various operational modes within the Atomsk program. After command-line arguments are parsed (by `read_cla.f90`), the main Atomsk routine calls `RUN_MODE` from this module. `RUN_MODE` then executes the specific logic associated with the requested mode, such as file conversion, atomic structure creation, multi-file processing (merging, averaging, interpolating), or various structural/property analyses (RDF, Nye tensor, local symmetry, etc.). It orchestrates the reading of input files, application of options, execution of mode-specific calculations or manipulations, and handling of output.

## Key Components

- **`MODULE modes`**: The main module definition.

- **`SUBROUTINE RUN_MODE(mode, options_array, outfileformats, pfiles, mode_param)`**:
    - This is the primary subroutine that directs Atomsk's workflow based on the user's command-line input.
    - **Inputs**:
        - `mode (CHARACTER(LEN=*), INTENT(IN))`: A string specifying the main operation Atomsk should perform (e.g., "normal", "create", "list", "diff", "rdf"). This is determined by `read_cla.f90`.
        - `options_array (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(IN))`: An array of strings, where each string contains a processing option and its parameters (e.g., "-select above 0.5 x").
        - `outfileformats (CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`: An allocatable array of 5-character strings indicating the desired output file formats. This can be modified by `RUN_MODE` (e.g., by `SET_OUTPUT`).
        - `pfiles (CHARACTER(LEN=4096), DIMENSION(5), INTENT(IN))`: An array containing up to five primary filenames parsed from the command line. Their specific roles (input, output, listfile, etc.) depend on the `mode`.
        - `mode_param (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(IN))`: An array containing parameters specific to certain modes (e.g., for `--create`, this holds structure type, lattice constants, species; for `--rdf`, it holds R_max and dR).
    - **Logic**:
        - Initializes local variables for filenames and mode-specific parameters.
        - A large `SELECT CASE(mode)` structure handles the different operational modes:
            - **`normal`**: Standard file conversion. Calls `CONVERT_AFF` from `mode_normal.f90`. Determines input/output files and formats.
            - **`list`**: Processes a list of files. Calls `LIST_XYZ` from `mode_list.f90`.
            - **`ddplot`**: Creates input for DDplot. Reads two files, applies options, calls `WRITE_DD`.
            - **`merge`**: Merges multiple files. Parses parameters from `mode_param`, calls `MERGE_XYZ`.
            - **`create`**: Creates a new atomic structure. Parses parameters from `mode_param`, calls `CREATE_CELL`.
            - **`gather` (`--all-in-one`)**: Concatenates multiple snapshots from a list file into a multi-snapshot output. Calls `ALLINONE`.
            - **`unfold` (`--one-in-all`)**: Splits a multi-snapshot file into individual files. Calls format-specific `ONEINALL_*` routines.
            - **`unwrap`**: Unwraps atomic coordinates. Calls `UNWRAP_XYZ`.
            - **`polycrystal`**: Generates a polycrystalline structure. Calls `POLYCRYS`.
            - **`interpolate`**: Interpolates between two atomic structures. Calls `INTERPOLATE_XYZ`.
            - **`average`**: Averages atomic positions from multiple files. Calls `AVERAGE_XYZ`.
            - **`cpprop` (`--copy-properties`)**: Copies auxiliary properties between files. Calls `COPY_PROPERTIES`.
            - **`match-id`**: Matches atom IDs between two files. Calls `MATCHID_XYZ`.
            - **Analysis Modes**:
                - `diff`: Calculates displacement vectors. Calls `DIFF_XYZ`.
                - `edm`: Calculates electric dipole moments. Calls `E_DIPOLES`.
                - `PE` (`--electronic-polarization`): Calculates electronic polarization. Calls `E_POLA`.
                - `rdf`: Calculates Radial Distribution Function. Calls `RDF_XYZ`.
                - `nye`: Calculates Nye tensor. Calls `NYE_TENSOR`.
                - `density`: Calculates 1D/2D/3D density profiles. Calls `DENSITY_XYZ`.
                - `cs` or `localsym`: Calculates local/central symmetry parameters. Calls `LOCAL_SYM`.
        - For many modes, especially analysis modes, it involves reading input file(s) using `READ_AFF`, applying common options using `OPTIONS_AFF`, and then calling the mode-specific routine.
        - The `CHECK_OPTION_WRAP` subroutine is called for several analysis modes to ensure atoms are wrapped into the simulation cell if necessary.

- **`SUBROUTINE CHECK_OPTION_WRAP(options_array)`**:
    - **Input/Output**: `options_array (CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE, INTENT(INOUT))`.
    - **Description**: Checks if the "-wrap" option is present in `options_array`. If not, it issues a warning (message ID 4712) and interactively prompts the user (reading from standard input) whether to add the "-wrap" option. If the user agrees, "-wrap" is added to the beginning of `options_array`. This is used to ensure that atomic coordinates are properly handled for calculations that require atoms to be within the primary simulation cell.

## Important Variables/Constants

The primary inputs to `RUN_MODE` ( `mode`, `options_array`, `outfileformats`, `pfiles`, `mode_param`) are the most crucial, as they dictate the entire workflow. This module itself does not define new public constants but orchestrates actions based on these inputs.

## Usage Examples

The `RUN_MODE` subroutine is the main workhorse called by `atomsk.f90` after the command-line arguments have been parsed by `read_cla.f90`. Its usage is internal to Atomsk's execution flow.

```fortran
! Conceptual call from atomsk.f90:
PROGRAM ATOMSK
  USE modes
  USE read_cla ! To get the parameters for RUN_MODE
  ! ... other modules ...
  IMPLICIT NONE

  CHARACTER(LEN=12) :: main_mode
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: cla_options, mode_specific_params
  CHARACTER(LEN=5), DIMENSION(:), ALLOCATABLE :: output_file_formats
  CHARACTER(LEN=4096), DIMENSION(5) :: primary_filenames
  CHARACTER(LEN=4096), DIMENSION(:), ALLOCATABLE :: all_cmd_args

  ! ... (Code to get command line arguments into all_cmd_args) ...
  ! ... (Allocate cla_options based on number of arguments) ...

  CALL GET_CLA(all_cmd_args, main_mode, cla_options, output_file_formats, &
               & primary_filenames, mode_specific_params)

  IF (nerr == 0) THEN
    CALL RUN_MODE(main_mode, cla_options, output_file_formats, &
                  & primary_filenames, mode_specific_params)
  END IF

  ! ... (Error handling and cleanup) ...
END PROGRAM ATOMSK
```

## Dependencies and Interactions

The `modes` module is a high-level coordinator and thus depends on a large number of other modules in Atomsk:
- **Core Utilities**: `atoms`, `comv` (for global variables like `nerr`, `verbosity`, `langyes`, `langBigYes`), `constants`, `strings`, `crystallography`, `messages`, `files`, `resize`, `subroutines`.
- **Input/Output & Options**: `readin` (for `READ_AFF`), `options` (for `OPTIONS_AFF`), `deterH`, `guess_form` (for `GUESS_FORMAT` used by `RUN_MODE` and various sub-modules).
- **Specific Mode Modules**:
    - `mode_normal`, `mode_list`, `out_dd`, `mode_merge`, `mode_create`, `mode_unwrap`, `mode_average`, `mode_cpprop`, `mode_matchid`.
    - One-in-all (`oia_*`): `oia_dlp_history`, `oia_qeout`, `oia_vaspout`, `oia_lmc`, `oia_xsf`, `oia_xyz`.
    - All-in-one (`aio`): `ALLINONE`.
    - Analysis/Computation: `mode_localsym`, `mode_density`, `mode_difference`, `edm` (for `E_DIPOLES`), `mode_epola` (for `E_POLA`), `mode_interpolate`, `mode_nye`, `mode_polycrystal`, `mode_rdf`.
- **Data Flow**: Typically, for modes involving file processing, `RUN_MODE` will coordinate calls to `READ_AFF` to load atomic data, then `OPTIONS_AFF` to apply any user-specified modifications, followed by the mode-specific routine which might perform calculations, further manipulations, and/or call output routines (often via `WRITE_AFF` from `writeout.f90`, though `WRITE_AFF` itself is not directly called from `RUN_MODE` but by the sub-mode modules like `CONVERT_AFF`).
- **Error Handling**: The `nerr` global variable (from `comv`) is checked or set by many of the called subroutines. `RUN_MODE` itself may also set `nerr` if mode parameters are incorrect or a mode is unrecognized.
- **Interactive Prompts**: `CHECK_OPTION_WRAP` provides an example of interactive user input if a potentially necessary option (`-wrap`) is missing for certain analysis modes.
