# `readconf.f90`

## Overview

The `readconf.f90` module is designed to read and process Atomsk's configuration files (typically named `atomsk.conf` on Unix-like systems or `atomsk.ini` on Windows). These files allow users to set default values for various program parameters, which can simplify command-line usage by presetting common options. Atomsk usually reads these configuration files from multiple locations (system-wide, user-specific, and current directory), with settings from later files overriding those from earlier ones. This module handles the parsing of these files, interpreting recognized keywords, and updating corresponding global program variables.

## Key Components

- **`MODULE readconf`**: The main module containing the configuration file reading logic.

- **`SUBROUTINE READ_CONF(conffile)`**:
    - **Input**: `conffile (CHARACTER(LEN=128))` - The full path to the configuration file to be read.
    - **Description**: This subroutine opens the specified `conffile`, reads it line by line, and parses each line for keywords and their values.
        - Lines starting with a `#` symbol are treated as comments and ignored.
        - Empty lines are also ignored.
        - For valid lines, the first word is extracted as a `keyword` (and converted to lowercase). The remainder of the line is then parsed as the value(s) for that keyword.
        - A `SELECT CASE` structure is used to handle recognized keywords. Based on the keyword, the associated value is read and used to set a corresponding global program variable (typically defined in the `comv` module).
        - **Supported Keywords and associated global variables (from `comv`)**:
            - `verbosity`: Sets `verbosity` (integer).
            - `format`: Adds the specified file extension (e.g., "xsf", "cfg") to a list of default output formats. *Note: The implementation in this subroutine uses a local `outfileformats` array and calls `SET_OUTPUT` from the `files` module. The effect of this on a global default format list is unclear as the local array is not passed out.*
            - `ow` or `overw`: Sets `overw` (logical flag for overwriting files).
            - `ig` or `ignore`: Sets `ignore` (logical flag for ignoring existing files).
            - `lang` or `language`: Sets `lang` (character string for message language, e.g., "en", "fr").
            - `nthreads` or `threads`: Sets the number of OpenMP threads via `OMP_SET_NUM_THREADS`.
            - `neigh*` or `*_search` (e.g., `neigh_search`): Sets `neighsearch` (character string for neighbor search algorithm preference).
            - `colour` or `color`: Sets `colourtext` (logical flag for enabling/disabling colored console output).
            - `colour_default`, `color_default`: Sets `colourdef` (string for default text color).
            - `colour_warning`, `color_warning`: Sets `colourwarn` (string for warning text color).
            - `colour_error`, `color_error`: Sets `colourerr` (string for error text color).
            - `progressbar`: Sets `progressbar` (string for the style of progress bar).
        - Certain keywords like "density", "nye", "orthocell" are explicitly recognized but skipped, as they are typically handled by specific modes or options rather than as global defaults.
        - Unrecognized keywords result in a warning message being displayed via `ATOMSK_MSG`.
        - Errors during reading a value (e.g., non-numeric string where a number is expected) also trigger a warning.

## Important Variables/Constants

- **`conffile (CHARACTER(LEN=128), INTENT(IN))`**: The primary input, specifying the path to the configuration file.
- **Global Variables from `comv` module (modified by this subroutine)**:
    - `verbosity (INTEGER)`
    - `overw (LOGICAL)`
    - `ignore (LOGICAL)`
    - `lang (CHARACTER(LEN=2))`
    - `Nthreads (INTEGER)` (indirectly by calling `OMP_SET_NUM_THREADS`)
    - `neighsearch (CHARACTER(LEN=16))`
    - `colourtext (LOGICAL)`
    - `colourdef (CHARACTER(LEN=16))`
    - `colourwarn (CHARACTER(LEN=16))`
    - `colourerr (CHARACTER(LEN=16))`
    - `progressbar (CHARACTER(LEN=16))`
    - `nwarn (INTEGER)`: Incremented when warnings occur.

## Usage Examples

The `READ_CONF` subroutine is called by the main Atomsk program during its initialization phase for each standard configuration file location.

**Conceptual call within Atomsk's main program:**
```fortran
PROGRAM ATOMSK_MAIN
  USE readconf
  ! ... other USE statements ...
  IMPLICIT NONE

  ! Order of reading config files (later files override earlier ones)
  CALL READ_CONF("/etc/atomsk.conf")      ! System-wide
  CALL READ_CONF(user_config_path)       ! User-specific (e.g., ~/.config/atomsk.conf)
  CALL READ_CONF("./atomsk.conf")        ! Local directory

  ! ... rest of Atomsk program ...
END PROGRAM ATOMSK_MAIN
```

**Example content of an `atomsk.conf` file:**
```
# This is a configuration file for Atomsk
verbosity 2       # Set default verbosity to level 2
format xsf        # Always output XSF by default (but see notes below)
nthreads 4        # Use 4 OpenMP threads if available
language fr       # Set messages to French
color Yes         # Enable colored output
color_warning yellow # Warnings in yellow
```

## Dependencies and Interactions

- **`comv`**: Essential for accessing and modifying global program settings variables (like `verbosity`, `overw`, `lang`, etc.) and for error/warning counters (`nwarn`, `nerr`).
- **`constants`**: Included with `USE constants`, likely for the `dp` kind parameter or other fundamental constants, though not directly used in `READ_CONF`'s logic.
- **`strings`**: Uses the `StrDnCase` function to make keyword matching case-insensitive.
- **`files`**: The `SET_OUTPUT` subroutine from the `files` module is called when the "format" keyword is encountered.
    - *Important Note*: The `outfileformats` array passed to `SET_OUTPUT` within `READ_CONF` is a local, uninitialized, allocatable array. This means that any formats read via the "format" keyword in the configuration file are added to this local array, which is then discarded when `READ_CONF` finishes. This behavior suggests that setting default output formats via the configuration file might not work as intended unless `outfileformats` in `comv` is directly manipulated or the local array's content is somehow propagated, which is not shown in the current code.
- **`messages`**: Uses `ATOMSK_MSG` for displaying warning messages (e.g., for unrecognized keywords or read errors).
- **OpenMP Library**: If Atomsk is compiled with OpenMP support, the `OMP_SET_NUM_THREADS` routine is called when the "nthreads" keyword is processed.
- **File System**: The subroutine interacts with the file system by attempting to `OPEN` and `READ` the `conffile` specified in its argument. Errors during file operations are handled.
- **Error Handling**: The subroutine includes `ERR=` and `END=` labels in `READ` statements to catch issues during parsing and reports them as warnings, incrementing `nwarn`. It does not typically stop the program for configuration file errors.
