# `strings.f90`

## Overview

The `strings.f90` module provides a collection of utility functions and subroutines for common string manipulations and conversions required within the Atomsk program. These utilities facilitate tasks such as converting real numbers to clean string representations, changing the case of strings, abbreviating long file paths for display, checking if a string can be interpreted as a number, removing or replacing specific characters within strings, converting string representations of booleans (like "true", "yes", "0") to logical values, and interpreting specialized string expressions involving keywords like "BOX" or percentages relative to a given box dimension.

## Key Components

- **`MODULE strings`**: The main module encapsulating all string utility functions.

- **`FUNCTION REAL2TSTR(R) RESULT(Rstring)`**:
    - Converts a double-precision real number `R` into a `CHARACTER(LEN=64)` string `Rstring`.
    - The output string is formatted to remove unnecessary trailing zeros after the decimal point and leading/trailing spaces.

- **`FUNCTION StrUpCase(input_string) RESULT(UC_string)`**:
    - Takes an `input_string` of any length and returns a new string `UC_string` of the same length where all lowercase English alphabet characters are converted to uppercase.

- **`FUNCTION StrDnCase(input_string) RESULT(lc_string)`**:
    - Takes an `input_string` of any length and returns a new string `lc_string` of the same length where all uppercase English alphabet characters are converted to lowercase.

- **`FUNCTION CHARLONG2SHRT(longname) RESULT(shortname)`**:
    - Shortens a given `longname` (typically a file path) to a `CHARACTER(LEN=64)` string `shortname`.
    - If `longname` is longer than 64 characters, it attempts to abbreviate it by inserting "..." in the middle, preserving initial path components and the final filename segments. It uses the `pathsep` variable (from `comv` module) to identify path components.

- **`LOGICAL FUNCTION STR_REAL(string)`**:
    - Checks if the input `string` can be successfully parsed as a real number (either integer or floating-point).
    - Returns `.TRUE.` if the string represents a valid number, `.FALSE.` otherwise.

- **`SUBROUTINE STR_RMSPACE(string)`**:
    - Modifies the input `string (INTENT(INOUT))` by removing all occurrences of blank space characters from it, effectively compacting the string.

- **`SUBROUTINE STR_CHAR2SPACE(string, characters)`**:
    - Modifies the input `string (INTENT(INOUT))` by replacing every character found within the `characters` input string with a blank space.

- **`SUBROUTINE STR2BOOL(string, bool)`**:
    - Converts a given `string` into a logical `bool (INTENT(OUT))` value.
    - Recognizes various string representations for true (e.g., "y", "yes", "true", "t", "1", "on") and false (e.g., "n", "no", "false", "f", "0", "off"), case-insensitively.
    - If the string is not recognized, it increments the global error counter `nerr` (from `comv` module).

- **`SUBROUTINE BOX2DBLE(vbox, text, number, status)`**:
    - Interprets a `text` string that may contain symbolic expressions related to a box dimension `vbox(3)`.
    - `vbox (REAL(dp), DIMENSION(3), INTENT(IN))`: Components of a box vector or dimensions.
    - `text (CHARACTER(LEN=128), INTENT(IN))`: The string to interpret (e.g., "BOX", "0.5*BOX", "BOX/2", "75%", "INF", "-INF").
    - `number (REAL(dp), INTENT(OUT))`: The resulting numerical value.
    - `status (INTEGER, INTENT(OUT))`: Returns 0 for success, 1 for failure in parsing.
    - Handles "INF" (infinity, using `HUGE(1.0_dp)`), "BOX" (evaluates to `MAX(0.0_dp,MAXVAL(vbox)) + DABS(MIN(0.0_dp,MINVAL(vbox)))` or `SUM(vbox)` depending on context/operator), and expressions like `<num>*BOX`, `BOX/<num>`, `<num> %` (percentage of `SUM(vbox)`). It's designed for simple expressions with one operator.

## Important Variables/Constants

- **`pathsep (CHARACTER(LEN=1))`**: A global variable from the `comv` module representing the file path separator (e.g., '/' or '\'), used by `CHARLONG2SHRT`.
- **`nerr (INTEGER)`**: A global error counter from the `comv` module, incremented by `STR2BOOL` if string conversion to boolean fails.

## Usage Examples

```fortran
USE strings
USE comv ! For dp, pathsep, nerr
IMPLICIT NONE

CHARACTER(LEN=64) :: num_as_string, shortened_name
CHARACTER(LEN=100) :: test_string, long_file_path
LOGICAL :: is_a_number, boolean_value
REAL(dp) :: real_value_from_box
REAL(dp), DIMENSION(3) :: cell_dim_x = [10.0_dp, 0.0_dp, 0.0_dp]
INTEGER :: error_status

! REAL2TSTR
num_as_string = REAL2TSTR(123.45600_dp) ! Expected: "123.456" (implementation might vary slightly)
PRINT *, "REAL2TSTR(123.45600): '", TRIM(num_as_string), "'"

! StrUpCase / StrDnCase
test_string = "AtomSK Test"
PRINT *, "StrUpCase: '", StrUpCase(test_string), "'"   ! Expected: ATOMSK TEST
PRINT *, "StrDnCase: '", StrDnCase(test_string), "'"   ! Expected: atomsk test

! CHARLONG2SHRT
pathsep = '/' ! Set pathsep for example consistency
long_file_path = "/usr/local/share/very_long_directory_name/data/file.dat"
shortened_name = CHARLONG2SHRT(long_file_path)
PRINT *, "CHARLONG2SHRT: '", TRIM(shortened_name), "'" ! E.g., "/usr/.../data/file.dat"

! STR_REAL
is_a_number = STR_REAL(" -123.45e-2 ")
PRINT *, "STR_REAL('-123.45e-2'): ", is_a_number ! Expected: .TRUE.
is_a_number = STR_REAL("NotANumber")
PRINT *, "STR_REAL('NotANumber'): ", is_a_number   ! Expected: .FALSE.

! STR_RMSPACE
test_string = "  Spaces   Be   Gone  ! "
CALL STR_RMSPACE(test_string)
PRINT *, "STR_RMSPACE: '", TRIM(test_string), "'" ! Expected: "SpacesBeGone!"

! STR_CHAR2SPACE
test_string = "replace_underscores_and-hyphens"
CALL STR_CHAR2SPACE(test_string, "_-")
PRINT *, "STR_CHAR2SPACE: '", test_string, "'" ! Expected: "replace underscores and hyphens"

! STR2BOOL
CALL STR2BOOL("TRUE", boolean_value)
PRINT *, "STR2BOOL('TRUE'): ", boolean_value ! Expected: T
CALL STR2BOOL("off", boolean_value)
PRINT *, "STR2BOOL('off'): ", boolean_value  ! Expected: F

! BOX2DBLE
CALL BOX2DBLE(cell_dim_x, "0.5*BOX", real_value_from_box, error_status)
IF (error_status == 0) PRINT *, "BOX2DBLE('0.5*BOX'): ", real_value_from_box ! Expected: 5.0
CALL BOX2DBLE(cell_dim_x, "BOX-1.0", real_value_from_box, error_status)
IF (error_status == 0) PRINT *, "BOX2DBLE('BOX-1.0'): ", real_value_from_box ! Expected: 9.0 (based on SUM(vbox) for +/-)
CALL BOX2DBLE(cell_dim_x, "20%", real_value_from_box, error_status)
IF (error_status == 0) PRINT *, "BOX2DBLE('20%'): ", real_value_from_box     ! Expected: 2.0

END PROGRAM example_strings
```

## Dependencies and Interactions

- **`comv`**: This module is `USE`d to import:
    - `dp`: The double precision kind parameter.
    - `pathsep`: The character used as a path separator (e.g., `/` or `\`), utilized by `CHARLONG2SHRT`.
    - `nerr`: The global error counter, which is incremented by `STR2BOOL` upon a conversion failure.
- **Fortran Intrinsic Functions**: The module makes extensive use of Fortran's intrinsic string manipulation functions (`TRIM`, `ADJUSTL`, `ADJUSTR`, `INDEX`, `SCAN`, `LEN`, `LEN_TRIM`), type conversion (`WRITE` to string, `READ` from string), and mathematical functions (`MAXVAL`, `MINVAL`, `DABS`, `SUM`, `HUGE`).
