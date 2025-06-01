# `expreval.f90`

## Overview

The `exprev.f90` module provides functionality to parse and evaluate mathematical expressions provided as strings. The core of the module is the recursive function `EXPREVAL`, which can interpret a string containing numbers, arithmetic operators, parentheses, and a predefined set of mathematical functions and constants, returning a single double-precision real value. The module also includes a helper subroutine `STR_EXP2VAL` for substituting named placeholders in a string with numerical values, although `EXPREVAL` itself doesn't directly support user-defined variables beyond its built-in constants.

The expression evaluator respects common operator precedence and supports functions like `cos`, `sin`, `exp`, `sqrt`, `log`, `abs`, etc., along with the constant `pi` and some physical constants (`kB`, `hbar`, etc.). It also includes a "rand" function for generating random numbers.

## Key Components

- **`MODULE exprev`**: The main module containing the expression evaluation logic.

- **`SUBROUTINE STR_EXP2VAL(string, expr, value, nr)`**:
    - **Inputs**:
        - `string (CHARACTER(LEN=*), INTENT(INOUT))`: The string in which substitutions are to be made.
        - `expr (CHARACTER(LEN=*), INTENT(IN))`: The substring (placeholder/variable name) to be replaced.
        - `value (REAL(dp), INTENT(IN))`: The numerical value to substitute for `expr`.
    - **Output**:
        - `string`: The modified string with `expr` replaced by the string representation of `value`.
        - `nr (INTEGER)`: The number of substitutions performed.
    - **Description**: Replaces all occurrences of `expr` in `string` with the character representation of `value`.

- **`RECURSIVE FUNCTION EXPREVAL(string, recuri, status) RESULT(eval)`**:
    - **Inputs**:
        - `string (CHARACTER(LEN=*), INTENT(INOUT))`: The mathematical expression string to be evaluated. This string is modified during the parsing process.
        - `recuri (INTEGER, INTENT(INOUT))`: Tracks the current recursion depth to prevent stack overflows (limited to 100).
    - **Output**:
        - `eval (REAL(dp))`: The numerical result of the evaluated expression.
        - `status (INTEGER)`: An error status code. 0 indicates success. Non-zero values indicate errors like excessive recursion, illegal characters, or division by zero.
    - **Description**:
        - Parses and evaluates the input `string` by recursively breaking it down according to operator precedence and function calls.
        - **Preprocessing**: Replaces `[]` and `{}` with `()`. Attempts to insert `*` for implicit multiplications (e.g., "2pi" becomes "2*pi", "3cos(x)" becomes "3*cos(x)").
        - **Recognized Operators**: `+`, `-`, `*`, `/` (or `:`), `%` (modulo), `^` (or `**` for power), `!` (factorial).
        - **Recognized Functions**: `sqrt`, `abs`, `int`, `acos`, `cos`, `asin`, `sin`, `atan`, `atan2`, `tan`, `exp`, `ln`, `log` (base 10).
        - **Recognized Constants**: `pi`, `kB` (Boltzmann constant), `hbar` (reduced Planck constant), `Navo` (Avogadro's number), `qe` (elementary charge), `epsilon0` (vacuum permittivity), `mu0` (vacuum permeability).
        - **Special Function**: `rand` or `random` generates a random number between 0 and 1.
        - **Error Handling**: Sets `status` to a non-zero value if parsing errors occur (e.g., illegal characters, division by zero, logarithm of non-positive number, factorial of negative number, recursion depth exceeded).
        - **Limitations**: As noted in the source code comments, the function may have issues with numerical precision due to intermediate string conversions, is not fully optimized for speed, and does not support complex numbers or user-defined variables directly within an expression string (these would need pre-substitution using `STR_EXP2VAL` or similar).

## Important Variables/Constants (within `EXPREVAL`)

- **`operators (CHARACTER(LEN=7))`**: Stores the string `"+-*/:^%"` defining valid arithmetic operators.
- **`illegal (CHARACTER(LEN=12))`**: Stores ` "&#'|`@°£$øµ§"` defining characters that will cause a parsing error.
- **`low_limit (REAL(dp), PARAMETER)`**: A small value (1.d-15) used to round off results of trigonometric functions that are very close to zero, potentially mitigating some floating-point inaccuracies.
- **`recuri (INTEGER)`**: Input/output argument tracking recursion depth. `EXPREVAL` exits with an error if `recuri` exceeds 100.
- **`status (INTEGER)`**: Output argument indicating success (0) or an error code.
- **`eval (REAL(dp))`**: The return value of the function, holding the numerical result of the expression.

## Usage Examples

```fortran
USE exprev
USE comv ! For dp
IMPLICIT NONE

REAL(dp) :: result_val
INTEGER :: error_code, current_recursion_depth
CHARACTER(LEN=256) :: math_expression

! Example 1: Basic arithmetic and function
math_expression = "sqrt(3.0**2 + 4.0**2) * sin(pi/6.0)" ! sqrt(9+16)*sin(pi/6) = 5 * 0.5 = 2.5
current_recursion_depth = 0
error_code = 0
result_val = EXPREVAL(math_expression, current_recursion_depth, error_code)

IF (error_code == 0) THEN
  PRINT *, "Expression: ", TRIM(math_expression)
  PRINT *, "Result: ", result_val  ! Expected: ~2.5
ELSE
  PRINT *, "Error evaluating '", TRIM(math_expression), "'. Status: ", error_code
END IF

! Example 2: Using STR_EXP2VAL for a placeholder
CHARACTER(LEN=256) :: template_expr = "2.0 * my_var + log(100.0)"
REAL(dp) :: var_value = 5.0
INTEGER :: num_replaced

CALL STR_EXP2VAL(template_expr, "my_var", var_value, num_replaced)
! template_expr is now something like "2.0 * 5.000000 + log(100.0)"

current_recursion_depth = 0
error_code = 0
result_val = EXPREVAL(template_expr, current_recursion_depth, error_code)

IF (error_code == 0) THEN
  PRINT *, "Substituted Expression: ", TRIM(template_expr)
  PRINT *, "Result: ", result_val ! Expected: 2.0 * 5.0 + 2.0 = 12.0
ELSE
  PRINT *, "Error evaluating '", TRIM(template_expr), "'. Status: ", error_code
END IF

! Example 3: Random number
math_expression = "10.0 * random"
current_recursion_depth = 0
error_code = 0
result_val = EXPREVAL(math_expression, current_recursion_depth, error_code)
IF (error_code == 0) THEN
    PRINT *, "10.0 * random: ", result_val ! Will be a value between 0 and 10
END IF

```

## Dependencies and Interactions

- **`comv`**: Provides the `dp` kind parameter for double-precision real numbers and the `verbosity` global variable (used for debug prints within `EXPREVAL` if `verbosity == 4`).
- **`constants`**: Supplies the values for predefined constants like `pi`, `kB` (Boltzmann constant), `hbar` (reduced Planck constant), `Navo` (Avogadro's number), `qe` (elementary charge), `eps_0` (vacuum permittivity), and `mu_0` (vacuum permeability).
- **`messages`**: This module is `USE`d, but `ATOMSK_MSG` is not directly called within `EXPREVAL` itself. It might be used by other functions in a larger context or was intended for future use.
- **`random`**: The `GEN_NRANDNUMBERS` subroutine from this module is called when the "rand" or "random" token is encountered in an expression string.
- **Fortran Intrinsics**: `EXPREVAL` uses various intrinsic functions like `TRIM`, `ADJUSTL`, `INDEX`, `SCAN`, `LEN_TRIM`, `NINT`, `DABS`, `DSQRT`, `DCOS`, `DSIN`, `DTAN`, `DACOS`, `DASIN`, `DATAN`, `DATAN2`, `DEXP`, `DLOG` (natural logarithm), `DLOG10` (base-10 logarithm), `MOD`.
- **Recursion**: The `EXPREVAL` function is recursive. It calls itself to evaluate sub-expressions, such as those within parentheses or arguments to functions. The depth of recursion is limited to prevent stack overflow errors.
