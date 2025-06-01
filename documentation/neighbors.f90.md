# `neighbors.f90`

## Overview

The `neighbors.f90` module provides a suite of subroutines dedicated to finding and managing lists of neighboring atoms within an atomic configuration. This is a fundamental requirement for many types of atomistic simulations and analyses. The module offers different algorithms for constructing neighbor lists, including a simple Verlet list method and a more efficient cell decomposition method for larger systems. It also includes utilities to retrieve the coordinates of these neighbors, find a specific number of nearest neighbors (NNN), identify neighbors within a given radial shell, or specifically find only the first nearest neighbors. All routines correctly handle periodic boundary conditions.

## Key Components

- **`MODULE neighbors`**: The main module encapsulating all neighbor-finding functionalities.

- **`SUBROUTINE NEIGHBOR_LIST(H, A, R, NeighList)`**:
    - Acts as a dispatcher, automatically choosing between `VERLET_LIST` and `CELL_LIST` based on system size, cutoff radius `R`, and cell dimensions. The global `neighsearch` variable from `comv` can also force a specific algorithm (though a comment indicates it's temporarily forced to "verlet").
    - **Output**: `NeighList (INTEGER, DIMENSION(:,:), ALLOCATABLE)` - A 2D array where `NeighList(i,k)` stores the index of the k-th neighbor of atom `i`. Rows correspond to atoms, columns to their neighbors. Trailing zeros indicate no more neighbors for that atom. If no neighbors are found for any atom, `NeighList` is deallocated.

- **`SUBROUTINE VERLET_LIST(H, A, R, NeighList)`**:
    - Implements a straightforward Verlet neighbor search algorithm. It computes distances between all pairs of atoms (considering periodic images) and includes pairs within the cutoff radius `R`.
    - Scales as O(N^2), suitable for smaller systems.
    - OpenMP parallelized for the main pair-distance calculation loop.
    - Dynamically estimates and resizes the `NeighList` array.

- **`SUBROUTINE CELL_LIST(H, A, R, NeighList)`**:
    - Implements a cell decomposition (or linked-cell list) algorithm for neighbor searching, which scales linearly with system size (O(N)) and is more efficient for large systems.
    - The simulation box is divided into smaller cells based on the cutoff radius `R`. For each atom, neighbors are searched only in its own cell and adjacent (neighboring) cells.
    - OpenMP parallelized for the loop assigning atoms to cells and the main neighbor search loop.
    - Also dynamically estimates and resizes `NeighList`.

- **`SUBROUTINE NEIGHBOR_POS(H, A, V, NeighList_in, Use_NeighList, radius, PosList)`**:
    - Given a central atom's position `V` and its pre-computed `NeighList_in` (if `Use_NeighList` is `.TRUE.`), or by performing a full search up to `radius` (if `Use_NeighList` is `.FALSE.`), this routine populates `PosList`.
    - **Output**: `PosList (REAL(dp), DIMENSION(:,:), ALLOCATABLE)` - For each neighbor found, stores its Cartesian coordinates `(1:3)`, its distance to `V` `(4)`, and its original index in the global atom array `A` `(5)`. It correctly accounts for periodic images of neighbors, including images of the central atom `V` itself if they are within the `radius`.

- **`SUBROUTINE FIND_NNN(H, A, V, NNN, V_NN, Nlist, exceeds100)`**:
    - Finds the `NNN` (Number of Nearest Neighbors) to a given Cartesian coordinate `V`. Atom positions in `A` and `V` are expected to be Cartesian.
    - **Outputs**:
        - `V_NN (REAL(dp), DIMENSION(:,:), ALLOCATABLE)`: Stores the Cartesian coordinates of the `NNN` found neighbors.
        - `Nlist (INTEGER, DIMENSION(:), ALLOCATABLE)`: Stores the original indices (from array `A`) of these `NNN` neighbors.
        - `exceeds100 (LOGICAL)`: A flag set to `.TRUE.` if an internal temporary storage limit (100 for `V_100`) is exceeded during the initial search phase before final selection of `NNN`. This implies the returned list might not be the true NNN if more than 100 candidates were closer than the NNN-th distinct distance found so far.

- **`SUBROUTINE FIND_NNRdR(H, A, V, R_in, dR, V_NN, Nlist, exceeds100)`**:
    - Finds all neighbors to a point `V` that lie within a spherical shell defined by an inner radius `R_in` (exclusive, `distance > R_in`) and an outer radius `R_in + dR` (inclusive, `distance <= R_in + dR`).
    - **Outputs**: Similar to `FIND_NNN`, `V_NN` stores neighbor coordinates, `Nlist` their indices. `exceeds100` flags if an internal temporary storage (1000 for `V_100`) is surpassed.

- **`SUBROUTINE FIND_1NN(H, A, V, V_NN, Nlist, exceeds100)`**:
    - Specifically finds only the first nearest neighbors to a point `V`. This includes all atoms that are at the shortest distance (within a small tolerance) from `V`.
    - **Outputs**: Similar to `FIND_NNN`.

## Important Variables/Constants

- **`H (REAL(dp), DIMENSION(3,3), INTENT(IN))`**: The 3x3 matrix of simulation box basis vectors, defining periodic boundary conditions. Input to all subroutines.
- **`A (REAL(dp), DIMENSION(:,:), INTENT(IN))`**: A 2D array containing atomic data, where `A(i,1:3)` are the Cartesian coordinates of atom `i`. Input to all subroutines.
- **`R (REAL(dp), INTENT(IN))`**: The cutoff radius for neighbor searching in `NEIGHBOR_LIST`, `VERLET_LIST`, `CELL_LIST`.
- **`NeighList (INTEGER, DIMENSION(:,:), ALLOCATABLE, INTENT(OUT))`**: The primary output of general neighbor list construction routines (`NEIGHBOR_LIST`, `VERLET_LIST`, `CELL_LIST`). `NeighList(i, :)` contains indices of neighbors for atom `i`.
- **`V (REAL(dp), DIMENSION(:), INTENT(IN))`**: A 3D vector representing the Cartesian coordinate of a point for which neighbors are sought (in `FIND_NNN`, `FIND_NNRdR`, `FIND_1NN`, and `NEIGHBOR_POS`).
- **`V_NN (REAL(dp), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT))`**: Output array in specialized find routines, storing Cartesian coordinates of the found neighbors.
- **`Nlist (INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(OUT))`**: Output array in specialized find routines, storing the original indices of the found neighbors.
- **`Afrac (REAL(dp), DIMENSION(SIZE(A,1),SIZE(A,2)))`**: Internal array used in several routines to store atom positions in fractional coordinates, facilitating periodic image calculations.
- **`NNincrement (INTEGER, PARAMETER)`**: Value 2, used in `VERLET_LIST` (though commented out) and `CELL_LIST` (also commented out) as an intended increment for resizing neighbor lists if they become full. The actual resizing in `VERLET_LIST` and `CELL_LIST` is handled by `RESIZE_INTARRAY2` which might use a different strategy.

## Usage Examples

```fortran
USE neighbors
USE comv ! For dp, nerr
IMPLICIT NONE

REAL(dp), DIMENSION(3,3) :: box_vectors
REAL(dp), DIMENSION(:,:), ALLOCATABLE :: atom_positions
REAL(dp) :: cutoff_radius
INTEGER, DIMENSION(:,:), ALLOCATABLE :: all_neighbor_list
REAL(dp), DIMENSION(3) :: point_V
INTEGER :: num_nearest
REAL(dp), DIMENSION(:,:), ALLOCATABLE :: nearest_coords
INTEGER, DIMENSION(:), ALLOCATABLE :: nearest_indices
LOGICAL :: too_many_neighbors

! Example: Initialize box_vectors for a cubic box of 10 Angstroms
box_vectors = 0.0_dp
box_vectors(1,1) = 10.0_dp
box_vectors(2,2) = 10.0_dp
box_vectors(3,3) = 10.0_dp

! Example: Create 2 atoms
ALLOCATE(atom_positions(2,4))
atom_positions(1,:) = [1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp] ! Atom 1 at (1,1,1) type 1
atom_positions(2,:) = [2.0_dp, 2.0_dp, 2.0_dp, 1.0_dp] ! Atom 2 at (2,2,2) type 1

cutoff_radius = 2.0_dp ! Angstroms

! Construct a general neighbor list for all atoms
CALL NEIGHBOR_LIST(box_vectors, atom_positions, cutoff_radius, all_neighbor_list)

IF (ALLOCATED(all_neighbor_list)) THEN
  PRINT *, "Neighbor list constructed."
  IF (SIZE(all_neighbor_list,1) >= 1) THEN
    PRINT *, "Atom 1 has", COUNT(all_neighbor_list(1,:) > 0), " neighbors."
  ENDIF
  ! ... use all_neighbor_list ...
END IF

! Find 1 nearest neighbor to atom 1's position
point_V = atom_positions(1,1:3)
num_nearest = 1
CALL FIND_NNN(box_vectors, atom_positions, point_V, num_nearest, &
              & nearest_coords, nearest_indices, too_many_neighbors)

IF (ALLOCATED(nearest_coords)) THEN
  PRINT *, "Found ", SIZE(nearest_indices), " nearest neighbor(s) for atom 1."
  IF(SIZE(nearest_indices) > 0) THEN
      PRINT *, "Nearest neighbor index: ", nearest_indices(1)
      PRINT *, "Nearest neighbor coords: ", nearest_coords(1,1:3)
  ENDIF
END IF

! Deallocate arrays
IF (ALLOCATED(atom_positions)) DEALLOCATE(atom_positions)
IF (ALLOCATED(all_neighbor_list)) DEALLOCATE(all_neighbor_list)
IF (ALLOCATED(nearest_coords)) DEALLOCATE(nearest_coords)
IF (ALLOCATED(nearest_indices)) DEALLOCATE(nearest_indices)

END PROGRAM example_neighbors
```

## Dependencies and Interactions

- **`comv`**: Provides the `dp` kind parameter for double-precision reals, the global `neighsearch` string variable (which can influence the choice between Verlet and cell list algorithms in `NEIGHBOR_LIST`), `verbosity` for controlling message output levels, and `nerr` for error tracking.
- **`subroutines`**: Utilizes several general utility functions:
    - `VECLENGTH`: For calculating distances between atoms.
    - `CART2FRAC`: To convert Cartesian coordinates to fractional coordinates, essential for handling periodic boundary conditions.
    - `VOLUME_PARA`: Used in `VERLET_LIST` and `CELL_LIST` to estimate the system's density and thereby an initial guess for the size of neighbor list arrays.
    - `INVMAT`: Used in `VERLET_LIST` if direct calculation of fractional coordinates is needed (when pre-conversion to `Afrac` fails or is skipped).
- **`messages`**: Employs the `ATOMSK_MSG` subroutine for standardized console output, including informational messages, progress updates during long calculations, and error messages.
- **`resize`**: The `RESIZE_INTARRAY2` subroutine is used by `VERLET_LIST` and `CELL_LIST` to dynamically adjust the dimensions of the `NeighList` array to the actual maximum number of neighbors found. This helps optimize memory usage by trimming oversized arrays.
- **OpenMP**: Both `VERLET_LIST` and `CELL_LIST` (for the main atom loop) incorporate OpenMP directives (`!$OMP PARALLEL DO`) to enable parallel execution on multi-core processors. This can significantly reduce the wall-clock time for neighbor list construction in large systems. The `VERLET_LIST` uses an `!$OMP CRITICAL` section to ensure thread-safe updates to the shared `NeighList` and `NNeigh` arrays.
- **Periodic Boundary Conditions (PBC)**: All neighbor finding routines are designed to correctly account for PBCs. They achieve this by considering periodic images of atoms when calculating distances, typically by working with fractional coordinates internally.
- **Algorithm Choice**: The `NEIGHBOR_LIST` acts as a high-level interface that can choose between the O(N^2) `VERLET_LIST` and the O(N) `CELL_LIST` based on system size and other criteria, or a user-defined preference via `neighsearch`.
