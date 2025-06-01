# `atomsk.f90`

## Overview

`atomsk.f90` is the main program file for Atomsk, a command-line tool designed for creating, manipulating, and converting atomic systems. It serves as the entry point for the software, handling command-line arguments, initializing parameters, and invoking different modes of operation. The program supports a wide range of file formats used in ab initio calculations, classical potential simulations, and visualization. Additionally, Atomsk can perform various transformations on atomic structures, such as creating supercells, defining crystal planes, applying stress, and introducing dislocations. This file orchestrates the overall workflow of the Atomsk software package.

## Key Components

- **`PROGRAM atomsk`**: The main program entry point. It controls the overall execution flow, including initialization, command-line argument parsing, mode selection, and finalization.
- **`MODULE comv`**: Provides common variables used throughout the Atomsk program.
- **`MODULE constants`**: Defines mathematical and physical constants.
- **`MODULE date_time`**: Contains subroutines for fetching and manipulating date and time values.
- **`MODULE messages`**: Handles the display and logging of informational messages, warnings, and errors.
- **`MODULE subroutines`**: A collection of general-purpose subroutines used by various parts of Atomsk.
- **`MODULE readconf`**: Responsible for reading and parsing configuration files (e.g., `atomsk.conf`, `atomsk.ini`).
- **`MODULE read_cla`**: Handles the parsing and interpretation of command-line arguments passed to Atomsk.
- **`MODULE modes`**: Manages the different operational modes of Atomsk (e.g., file conversion, crystal building).
- **`MODULE mode_interactive`**: Implements the functionality for Atomsk's interactive mode.
- **`SUBROUTINE GET_CLA(...)`**: (from `read_cla` module) Parses command-line arguments to determine modes, options, input/output files, and other parameters.
- **`SUBROUTINE READ_CONF(filepath)`**: (from `readconf` module) Reads configuration parameters from the specified file.
- **`SUBROUTINE RUN_MODE(...)`**: (from `modes` module) Executes the specified operational mode with the given parameters and options.
- **`SUBROUTINE INTERACT()`**: (from `mode_interactive` module) Starts and manages the interactive session with the user.
- **`SUBROUTINE ATOMSK_MSG(...)`**: (from `messages` module) Displays or logs messages according to the current verbosity level.
- **`FUNCTION IARGC()`**: Intrinsic Fortran function to get the count of command-line arguments.
- **`SUBROUTINE GETARG(i, arg)`**: Intrinsic Fortran subroutine to get the i-th command-line argument.

## Important Variables/Constants

- **`mode (CHARACTER(LEN=12))`**: Stores the current operational mode of the program (e.g., "normal", "interactive", specific processing modes like "--create"). Default is "normal" if arguments are provided, "interactive" otherwise.
- **`outfileformats (CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:))`**: An array holding the list of output file formats specified by the user through command-line arguments.
- **`options_array (CHARACTER(LEN=128), ALLOCATABLE, DIMENSION(:))`**: Stores the processing options and their parameters passed via command-line or configuration files (e.g., "-select", "-duplicate").
- **`pfiles (CHARACTER(LEN=4096), DIMENSION(5))`**: An array storing paths for primary input and output files. `pfiles(1)` is typically the main input, `pfiles(2)` the main output.
- **`verbosity (INTEGER)`**: Controls the level of detail in program messages.
    - `0`: Silent (no messages).
    - `1`: Messages to screen only (default).
    - `2`: Messages to log file only.
    - `3`: Messages to both screen and log file.
    - `4`: Debug mode with additional messages to log file.
- **`logfile (CHARACTER(LEN=128))`**: Name of the log file. Default is "atomsk.log". Can be changed with the `-log` option.
- **`overw (LOGICAL)`**: If `.TRUE.`, Atomsk will overwrite existing output files. Set by the `-overwrite` flag. Default is `.FALSE.`.
- **`ignore (LOGICAL)`**: If `.TRUE.`, Atomsk will skip operations if the output file already exists (used in some conversion modes). Set by the `-ignore` flag. Default is `.FALSE.`.
- **`Nthreads (INTEGER)`**: Stores the number of OpenMP threads to be used for parallel computations. Set by the `-Nthreads` option. Default is 0 (system decides or serial execution if OpenMP is not enabled).
- **`homedir (CHARACTER(LEN=128))`**: A utility variable used to construct paths to system-wide, user-specific, and local configuration files.
- **`clarg (CHARACTER(LEN=4096))`**: A temporary variable used to hold individual command-line arguments during parsing.
- **`nwarn (INTEGER)`**: Counter for warning messages generated during execution. (Used by `comv` module)
- **`nerr (INTEGER)`**: Counter for error messages generated during execution. If `nerr > 0` at certain checkpoints, the program may terminate. (Used by `comv` module)
- **`lang (CHARACTER(LEN=2))`**: Specifies the language for messages. Default is 'en'. Can be influenced by the `LANG` environment variable or the `-lang` option. (Used by `comv` module)

## Usage Examples

The `atomsk.f90` file is the main executable. Its usage is through command-line interface. The internal logic for argument parsing (via `IARGC()`, `GETARG()`, and the loop processing `clarg`) determines the program's behavior based on these inputs.

```fortran
! This is a conceptual representation of how atomsk might be called from a command line.
! These examples illustrate how command-line arguments are processed by atomsk.f90.

! Example 1: Convert 'input.cfg' to 'output.xsf' format.
! > atomsk input.cfg output.xsf
! This would involve pfiles(1) = "input.cfg", outfileformats(1) = "xsf" (implicitly or explicitly).

! Example 2: Run in create mode, reading parameters from 'crystal_params.txt',
!            apply an option, and save to 'final_structure.xyz'.
! > atomsk --create Si diamond 5.43 -option name_param value output.xyz
! 'mode' would be "--create", 'mode_param' would hold crystal parameters,
! 'options_array' would contain "-option name_param value", 'pfiles(1)' becomes "output.xyz".

! Example 3: Get help for the 'options' section.
! > atomsk --help options
! 'helpsection' variable would be set to "options", leading to display of help for options.

! Example 4: Set verbosity to level 3 (screen and log) and specify a custom log file.
! > atomsk -v 3 -log my_run.log input.dat output.dat
! 'verbosity' becomes 3, 'logfile' becomes "my_run.log".

! Example 5: Read options from a file instead of the command line.
! > atomsk input.gin -options options_file.txt output.xyz
! The content of 'options_file.txt' will be parsed and inserted into 'cla' array for processing.
```

## Dependencies and Interactions

- **Internal Modules**: `atomsk.f90` is the central orchestrator and depends heavily on the Fortran modules it `USE`s:
    - `comv`: For shared global variables (like `nwarn`, `nerr`, `verbosity`, `lang`).
    - `constants`: For predefined physical or mathematical constants.
    - `date_time`: For logging timestamps.
    - `messages`: For displaying all user-facing messages, warnings, and errors.
    - `subroutines`: For various utility functions.
    - `readconf`: To load settings from configuration files.
    - `read_cla`: To parse arguments provided on the command line.
    - `modes`: To execute the main operations of Atomsk based on the chosen mode.
    - `mode_interactive`: To handle the interactive mode session.
- **Configuration Files**: Atomsk reads settings in a specific order of precedence:
    1. System-wide configuration file (e.g., `/etc/atomsk.conf` on Linux, disabled on Windows).
    2. User-specific configuration file (e.g., `~/.config/atomsk.conf` or `$XDG_CONFIG_HOME/atomsk.conf` on Linux, `%HOMEPATH%\atomsk.ini` on Windows).
    3. A configuration file named `atomsk.conf` (Linux) or `atomsk.ini` (Windows) in the current working directory.
- **Command-Line Arguments**: The primary way users interact with Atomsk. `atomsk.f90` uses intrinsic `IARGC()` and `GETARG()` and then the `read_cla` module to parse these arguments, which dictate input/output files, operational modes (e.g., `--create`, `--polycrystal`), and options (e.g., `-duplicate`, `-rotate`, `-select`).
- **File System**: The program interacts extensively with the file system to:
    - Read input atomic structure files.
    - Write output atomic structure files in specified formats.
    - Read configuration files.
    - Write log files (if `verbosity` level implies it).
- **Environment Variables**:
    - `HOMEPATH` (Windows) or `HOME`, `XDG_CONFIG_HOME` (Linux/Unix): Used to locate user-specific configuration files.
    - `LANG`: Can influence the language of output messages if a corresponding translation is available.
    - `USER` (Linux/Unix): Used to get the username, for instance, to display a warning if running as root.
- **OpenMP (Optional)**: If Atomsk is compiled with OpenMP support (indicated by the `OPENMP` preprocessor directive in the code), it can perform certain calculations in parallel. The number of threads can be controlled by the `-Nthreads` command-line option, which internally calls `OMP_SET_NUM_THREADS`.
- **Standard Output/Error**: Messages, prompts, and errors are written to standard output or standard error streams, managed by the `messages` module and controlled by the `verbosity` setting. If output is redirected (e.g. `atomsk ... > fileout`), screen messages might be suppressed or handled differently.
