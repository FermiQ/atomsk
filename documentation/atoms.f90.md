# `atoms.f90`

## Overview

The `atoms.f90` module provides a set of utility subroutines for querying fundamental atomic properties. It serves as a centralized lookup for converting between atomic species symbols (e.g., "H", "Fe", "Si") and their corresponding atomic numbers (Z). It also provides functions to retrieve standard atomic masses based on the species symbol. Conversely, it can determine an atomic species symbol given its atomic number or, with some ambiguity, its atomic mass. These functions are essential for interpreting chemical information within the Atomsk program.

## Key Components

- **`MODULE atoms`**: The main module containing all related subroutines and constants.
- **`ATOMMAXZ (PUBLIC INTEGER, PARAMETER)`**: A public constant defining the maximum atomic number (currently 118 for Oganesson) for which information is stored in this module.
- **`SUBROUTINE ATOMNUMBER(species, snumber)`**:
    - **Input**: `species (CHARACTER(LEN=2))` - The chemical symbol of the atom (e.g., "Fe").
    - **Output**: `snumber (REAL(dp))` - The atomic number corresponding to the input species.
    - **Description**: Converts a 2-character atomic symbol to its atomic number. It handles case normalization for the input symbol (e.g., "fe", "fE" are treated as "Fe"). If the species is not recognized, `snumber` is set to 0.0.
- **`SUBROUTINE ATOMMASS(species, smass)`**:
    - **Input**: `species (CHARACTER(LEN=2))` - The chemical symbol of the atom.
    - **Output**: `smass (REAL(dp))` - The standard atomic mass (in AMU) for the input species.
    - **Description**: Retrieves the atomic mass for a given 2-character atomic symbol. Case normalization is applied to the input `species`. Atomic masses are based on NIST data. If the species is not recognized, `smass` is set to 0.0.
- **`SUBROUTINE ATOMSPECIES(snumber, species)`**:
    - **Input**: `snumber (REAL(dp))` - The atomic number.
    - **Output**: `species (CHARACTER(LEN=2))` - The chemical symbol corresponding to the atomic number.
    - **Description**: Converts an atomic number (integer part is used) to its 2-character chemical symbol. If the atomic number is outside the known range (1 to `ATOMMAXZ`), `species` is set to "XX".
- **`SUBROUTINE ATOMMASSSPECIES(smass, species)`**:
    - **Input**: `smass (REAL(dp))` - The atomic mass (in AMU).
    - **Output**: `species (CHARACTER(LEN=2))` - The chemical symbol approximately corresponding to the mass.
    - **Description**: Determines a chemical symbol by rounding the input `smass` to the nearest integer and looking up the corresponding element. This can be ambiguous for isotopes or elements with very similar integer masses (e.g., Ar and Ca around mass 40, Co and Ni around mass 59); the routine includes specific logic to handle some of these cases. If no species matches, `species` is set to "Xx".

## Important Variables/Constants

- **`ATOMMAXZ (INTEGER, PARAMETER)`**: Public constant, value 118. Defines the upper limit of atomic numbers recognized by the module.
- **`species (CHARACTER(LEN=2))`**: Used as input or output for the 2-letter chemical symbol of an element. The subroutines ensure proper casing (e.g., "Fe", not "fe" or "FE") for internal lookups or as output.
- **`snumber (REAL(dp))`**: Represents the atomic number, typically an integer value but stored as a double-precision real.
- **`smass (REAL(dp))`**: Represents the atomic mass in atomic mass units (AMU).

## Usage Examples

These subroutines are fundamental utilities used throughout Atomsk for handling atomic species information.

```fortran
USE atoms
IMPLICIT NONE

REAL(dp) :: atomic_z, iron_mass, silicon_mass
CHARACTER(LEN=2) :: symbol_from_z, symbol_from_mass_Fe, symbol_from_mass_Ar

! Get atomic number from species symbol
CALL ATOMNUMBER("Si", atomic_z)
PRINT *, "Atomic number of Si:", atomic_z  ! Output: 14.0

! Get atomic mass from species symbol
CALL ATOMMASS("Fe", iron_mass)
PRINT *, "Atomic mass of Fe:", iron_mass    ! Output: ~55.845
CALL ATOMMASS("Xx", silicon_mass) ! Unrecognized species
PRINT *, "Atomic mass of Xx:", silicon_mass ! Output: 0.0

! Get species symbol from atomic number
CALL ATOMSPECIES(26.0_dp, symbol_from_z)
PRINT *, "Species with Z=26:", symbol_from_z ! Output: Fe

CALL ATOMSPECIES(150.0_dp, symbol_from_z) ! Invalid Z
PRINT *, "Species with Z=150:", symbol_from_z ! Output: XX

! Get species symbol from atomic mass (approximate)
CALL ATOMMASSSPECIES(55.845_dp, symbol_from_mass_Fe)
PRINT *, "Species with mass ~55.8:", symbol_from_mass_Fe ! Output: Fe

CALL ATOMMASSSPECIES(39.95_dp, symbol_from_mass_Ar)
PRINT *, "Species with mass ~39.95:", symbol_from_mass_Ar ! Output: Ar
! (due to specific handling for mass 40, smass < 40.d0 -> Ar)
```

## Dependencies and Interactions

- **`comv`**: This module is used, likely to provide the definition for `dp` (the double precision kind parameter), ensuring consistent real number precision across the Atomsk codebase.
- **`strings`**: The `ATOMNUMBER` and `ATOMMASS` subroutines use `StrUpCase` (to convert the first letter to uppercase) and `StrDnCase` (to convert the second letter to lowercase) from the `strings` module. This standardizes the input `species` string before it's used in `SELECT CASE` statements, making the lookups case-insensitive.
- **Periodic Table Data**: The core functionality relies on hardcoded `SELECT CASE` blocks that map elemental symbols, atomic numbers, and atomic masses according to the conventional periodic table up to `ATOMMAXZ`. The atomic masses are noted to be from NIST.
