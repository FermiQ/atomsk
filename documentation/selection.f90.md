# `selection.f90`

## Overview

The `selection.f90` module provides utility functions for handling atom selections within Atomsk. Atom selections are a common feature in atomistic simulations and analysis, allowing operations to be applied to specific subsets of atoms. This module offers a way to check if a particular atom is part of the current selection and to generate a summarized list of all selected atoms, categorized by their chemical species.

## Key Components

- **`MODULE selection`**: The main module encapsulating selection-related utilities.

- **`LOGICAL FUNCTION IS_SELECTED(SELECT, i)`**:
    - **Inputs**:
        - `SELECT (LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(IN))`: An allocatable logical array representing the selection status of atoms. If `SELECT(k)` is `.TRUE.`, atom `k` is selected.
        - `i (INTEGER, INTENT(IN))`: The index of the atom to check.
    - **Output**: `(LOGICAL)` - Returns `.TRUE.` if atom `i` is selected, `.FALSE.` otherwise.
    - **Description**: This function determines the selection status of a given atom.
        - If the `SELECT` array is not allocated (meaning no specific selection criteria have been applied yet or all atoms are implicitly selected), the function returns `.TRUE.` for any valid atom index `i`.
        - If `SELECT` is allocated, the function returns the logical value stored at `SELECT(i)`.
        - If `i` is out of the bounds of an allocated `SELECT` array, it returns `.FALSE.`.

- **`SUBROUTINE LIST_SELECTED_ATOMS(P, SELECT, selectedlist)`**:
    - **Inputs**:
        - `P (REAL(dp), DIMENSION(:,:), INTENT(IN))`: The main atom data array, where `P(k,4)` stores the atomic number (or type) of atom `k`.
        - `SELECT (LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(IN))`: The logical array indicating the selection status of each atom, as used by `IS_SELECTED`.
    - **Output**:
        - `selectedlist (INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT))`: A 2D integer array summarizing the selected atoms. For each row `j`, `selectedlist(j,1)` is an atomic number, and `selectedlist(j,2)` is the count of selected atoms of that atomic number.
    - **Description**: This subroutine iterates through all atoms in the system. For each atom, it uses `IS_SELECTED` to check if it's part of the current selection. If an atom is selected, its species (atomic number from `P(:,4)`) is identified, and a counter for that species in a temporary list is incremented. After checking all atoms, the temporary list is compacted into the `selectedlist` output array, which will have `Nspecies` rows, where `Nspecies` is the number of distinct atomic species that have at least one atom selected.

## Important Variables/Constants

- **`SELECT (LOGICAL, DIMENSION(:), ALLOCATABLE, INTENT(IN))`**: This is the primary data structure representing atom selections. Its allocation status and content determine which atoms are considered "selected". It's typically populated by other parts of Atomsk based on user commands (e.g., `-select` option).
- **`P (REAL(dp), DIMENSION(:,:), INTENT(IN))`**: The array containing atom properties, critically `P(:,4)` for atomic numbers, used by `LIST_SELECTED_ATOMS` to categorize selected atoms by species.
- **`selectedlist (INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT))`**: The output of `LIST_SELECTED_ATOMS`, providing a summary (species and count) of the selection.

## Usage Examples

```fortran
USE selection
USE comv ! For dp definition
IMPLICIT NONE

REAL(dp), DIMENSION(5,4) :: atoms_data
LOGICAL, DIMENSION(5) :: current_selection
LOGICAL :: atom3_is_selected
INTEGER, DIMENSION(:,:), ALLOCATABLE :: selection_summary
INTEGER :: i

! Initialize atom data (x, y, z, atomic_number)
atoms_data(1,:) = [0.0_dp, 0.0_dp, 0.0_dp, 26.0_dp] ! Fe
atoms_data(2,:) = [1.0_dp, 0.0_dp, 0.0_dp, 26.0_dp] ! Fe
atoms_data(3,:) = [0.0_dp, 1.0_dp, 0.0_dp, 8.0_dp]  ! O
atoms_data(4,:) = [1.0_dp, 1.0_dp, 0.0_dp, 26.0_dp] ! Fe
atoms_data(5,:) = [0.5_dp, 0.5_dp, 0.5_dp, 8.0_dp]  ! O

! Define a selection: select atoms 1, 3, 5
current_selection = [.TRUE., .FALSE., .TRUE., .FALSE., .TRUE.]

! Check if atom 3 is selected
atom3_is_selected = IS_SELECTED(current_selection, 3)
PRINT *, "Is atom 3 selected?", atom3_is_selected ! Expected: .TRUE.

! Check if atom 2 is selected
PRINT *, "Is atom 2 selected?", IS_SELECTED(current_selection, 2) ! Expected: .FALSE.

! Get a summary of selected atoms
CALL LIST_SELECTED_ATOMS(atoms_data, current_selection, selection_summary)

IF (ALLOCATED(selection_summary)) THEN
  PRINT *, "Summary of selected atoms:"
  DO i = 1, SIZE(selection_summary, 1)
    PRINT *, "Atomic Number: ", selection_summary(i,1), " Count: ", selection_summary(i,2)
    ! Expected output (order might vary):
    ! Atomic Number: 26 Count: 1  (for atom 1)
    ! Atomic Number:  8 Count: 2  (for atoms 3 and 5)
  END DO
  DEALLOCATE(selection_summary)
END IF

! Example with no selection array allocated (all atoms considered selected)
DEALLOCATE(current_selection) ! Ensure it's not allocated
PRINT *, "Is atom 4 selected (no selection array)?", IS_SELECTED(current_selection, 4) ! Expected: .TRUE.
CALL LIST_SELECTED_ATOMS(atoms_data, current_selection, selection_summary)
IF (ALLOCATED(selection_summary)) THEN
  PRINT *, "Summary of all atoms (as selected):"
   DO i = 1, SIZE(selection_summary, 1)
    PRINT *, "Atomic Number: ", selection_summary(i,1), " Count: ", selection_summary(i,2)
    ! Expected output (order might vary):
    ! Atomic Number: 26 Count: 3
    ! Atomic Number:  8 Count: 2
  END DO
  DEALLOCATE(selection_summary)
END IF

END PROGRAM example_selection
```

## Dependencies and Interactions

- **`comv`**: This module is `USE`d, primarily for the `dp` kind parameter for double-precision real numbers. It does not directly modify or read other global variables from `comv`.
- **Atom Data Structure**: The `LIST_SELECTED_ATOMS` subroutine relies on the convention that the fourth column of the `P` array (`P(:,4)`) contains the atomic number (or a numerical representation of the species type).
- **Selection Mechanism**: The functionality of this module is tightly coupled with how atom selections are managed and represented elsewhere in Atomsk. The `SELECT` logical array is expected to be populated by other parts of the program that handle user commands for selecting atoms (e.g., based on position, species, or other criteria).
