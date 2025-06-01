# `sort.f90`

## Overview

The `sorting.f90` module provides a collection of subroutines for various array sorting and reordering tasks. These are primarily designed to operate on 2D arrays, typically representing lists of atomic coordinates or associated properties, where sorting is performed based on the values in a specified column. The module includes standard sorting algorithms like Bubble Sort and Quick Sort, a specialized "pack sort" to group identical elements, routines to reorder arrays based on a predefined index list, and a utility to match atoms between two different configurations based on species and proximity.

## Key Components

- **`MODULE sorting`**: The main module that encapsulates all sorting routines.

- **`SUBROUTINE BUBBLESORT(A, col, order, newindex)`**:
    - Sorts a 2D real array `A` based on the values in column `col`.
    - `order (CHARACTER(LEN=4))`: Specifies the sort direction ("up  " for ascending, "down" for descending).
    - `newindex (INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT))`: An array that stores the original row indices in their new, sorted order.
    - Implements the Bubble Sort algorithm, which has a time complexity of O(N^2) and is suitable for small arrays.

- **`SUBROUTINE QUICKSORT(A, col, order, newindex)`**:
    - Similar to `BUBBLESORT` but uses the Quick Sort algorithm, which generally has a time complexity of O(N*log(N)), making it more efficient for larger arrays.
    - It acts as a wrapper for the recursive subroutine `QSORT`.
    - `newindex` provides the mapping from the new sorted positions back to the original row indices.

- **`RECURSIVE SUBROUTINE QSORT(A, col, order, newindex)`**:
    - The recursive helper routine that implements the Quick Sort logic. It partitions the array and recursively sorts the sub-arrays.

- **`SUBROUTINE QS_PARTITION(A, col, order, marker, newindex)`**:
    - The partitioning subroutine used by `QSORT`. It selects a pivot element and rearranges the array (and `newindex` accordingly) such that elements smaller (or larger, depending on `order`) than the pivot come before it, and larger (or smaller) elements come after.
    - `marker (INTEGER, INTENT(OUT))`: Returns the position of the pivot after partitioning.

- **`SUBROUTINE PACKSORT(A, col, newindex)`**:
    - Sorts the 2D array `A` based on the values in column `col` by "packing" or grouping identical values together. The relative order of items with different values in the sort column is not strictly defined as ascending or descending, but all rows with the same value in column `col` become contiguous.
    - `newindex` stores the resulting order of original row indices.

- **`SUBROUTINE IDSORT(idlist, A)`**:
    - Reorders the rows of a 2D real array `A` according to the mapping provided in the integer array `idlist`. The `i`-th row of the modified `A` will be taken from the `idlist(i)`-th row of the original `A`.
    - Handles cases where `idlist(i)` might be 0 or out of bounds, with specific logic for placing atoms whose original positions are taken by others.

- **`SUBROUTINE IDSORT_SELECT(idlist, A)`**:
    - Similar to `IDSORT`, but specifically designed to reorder a 1D logical array `A` (e.g., an atom selection mask) based on the `idlist`.

- **`SUBROUTINE FIND_MATCHING_ID(P1, P2, idlist, Npaired)`**:
    - Attempts to find a unique matching atom in array `P2` for each atom in array `P1`.
    - Matching is based on two criteria:
        1.  The atoms must be of the same chemical species (determined from `P1(:,4)` and `P2(:,4)`).
        2.  The atom in `P2` must be the closest one to the atom in `P1` among all atoms of the same species in `P2`.
    - **Outputs**:
        - `idlist (INTEGER, DIMENSION(:), ALLOCATABLE)`: For each atom `i` in `P1`, `idlist(i)` stores the index of the matched atom in `P2`. If no match is found, `idlist(i)` is 0.
        - `Npaired (INTEGER)`: The total number of successful pairings made.
    - The routine includes logic to resolve situations where multiple atoms in `P1` might initially match the same atom in `P2`, by ensuring each atom in `P2` is matched to at most one atom in `P1` (the closest one).
    - An OpenMP parallel directive (`!!!!!$OMP PARALLEL DO`) is present but commented out for the main loop.

## Important Variables/Constants

- **`A (REAL(dp), DIMENSION(:,:), INTENT(INOUT))`**: The primary 2D array being sorted or reordered in most subroutines.
- **`col (INTEGER, INTENT(IN))`**: The column index used as the key for sorting in `BUBBLESORT`, `QUICKSORT`, and `PACKSORT`.
- **`order (CHARACTER(LEN=4), INTENT(IN))`**: A string ("up  " or "down") that specifies the sort order (ascending or descending) for `BUBBLESORT` and `QUICKSORT`.
- **`newindex (INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT))`**: An output array in `BUBBLESORT`, `QUICKSORT`, `PACKSORT` that stores the original indices of the rows in their new sorted positions.
- **`idlist (INTEGER, DIMENSION(:), INTENT(IN))`**: In `IDSORT` and `IDSORT_SELECT`, this input array defines the desired new order for the rows of array `A`.
- **`P1 (REAL(dp), DIMENSION(:,:), INTENT(IN))`**, **`P2 (REAL(dp), DIMENSION(:,:), INTENT(IN))`**: Input arrays in `FIND_MATCHING_ID`, representing two sets of atomic configurations. Column 4 (`P1(:,4)`, `P2(:,4)`) is assumed to hold the species identifier (e.g., atomic number).
- **`idlist (INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT))`**: In `FIND_MATCHING_ID`, this output array maps each atom in `P1` to its matched atom in `P2`.
- **`Npaired (INTEGER, INTENT(OUT))`**: In `FIND_MATCHING_ID`, this output variable gives the count of successfully matched pairs.

## Usage Examples

```fortran
USE sorting
USE comv ! For dp
IMPLICIT NONE

REAL(dp), DIMENSION(4,3) :: atom_data
INTEGER, DIMENSION(4) :: original_indices
INTEGER :: sort_column

! Initialize atom_data (e.g., x, y, z coordinates)
atom_data(1,:) = [10.0_dp, 2.0_dp, 5.0_dp]
atom_data(2,:) = [3.0_dp,  8.0_dp, 1.0_dp]
atom_data(3,:) = [7.0_dp,  1.0_dp, 6.0_dp]
atom_data(4,:) = [3.0_dp,  0.0_dp, 4.0_dp]

! Sort by the first column (x-coordinate) in ascending order
sort_column = 1
CALL QUICKSORT(atom_data, sort_column, "up  ", original_indices)

PRINT *, "Sorted data (by column 1 ascending):"
DO sort_column = 1, 4 ! Using sort_column as loop var here for printing
  PRINT *, atom_data(sort_column,:)
END DO
PRINT *, "Original row indices in new order:", original_indices
! Expected output:
! Sorted data (by column 1 ascending):
!    3.0000000000000000        8.0000000000000000        1.0000000000000000
!    3.0000000000000000        0.0000000000000000        4.0000000000000000
!    7.0000000000000000        1.0000000000000000        6.0000000000000000
!    10.000000000000000        2.0000000000000000        5.0000000000000000
! Original row indices in new order:           2           4           3           1

! Further examples could demonstrate PACKSORT, IDSORT, and FIND_MATCHING_ID
END PROGRAM example_sorting
```

## Dependencies and Interactions

- **`comv`**: Provides the `dp` kind parameter for double-precision real numbers.
- **`constants`**: This module is `USE`d, but its constants are not directly referenced in the sorting routines themselves.
- **`math`**: The `VECLENGTH` function from the `math` module is used in `FIND_MATCHING_ID` to calculate distances between atoms.
- **Algorithm Choice**: `BUBBLESORT` is simpler but less efficient (O(N^2)) than `QUICKSORT` (average O(N log N)). The choice of which to use would depend on the expected size of the arrays to be sorted.
- **In-place Sorting**: The sorting routines (`BUBBLESORT`, `QUICKSORT`, `PACKSORT`) modify the input array `A` directly (in-place).
- **Index Mapping**: The `newindex` array returned by sorting routines is crucial for tracking the original positions of rows if other associated data needs to be reordered consistently. `IDSORT` and `IDSORT_SELECT` use such an index list to perform the reordering.
