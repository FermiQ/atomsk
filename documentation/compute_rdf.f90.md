# `compute_rdf.f90`

## Overview

The `cmpt_rdf.f90` module is designed to calculate the Radial Distribution Function (RDF), often denoted as g(r), for a given atomic system. The RDF is a statistical mechanics tool that describes how the density of atoms varies as a function of distance from a reference atom. It provides insights into the local structure and ordering of materials.

If the system contains multiple atomic species (e.g., A and B), this module computes partial RDFs for all unique pairs of species (e.g., g_AA(r), g_AB(r), and g_BB(r)). The calculation involves:
1.  For each atom in the system, counting the number of neighboring atoms located within a series of concentric spherical shells (or "skins") of radius R and thickness dR.
2.  Averaging these counts over all atoms of a given reference species.
3.  Normalizing these averaged counts by the expected number of atoms in an ideal gas of the same bulk density occupying the same shell volume.

The module can either use a pre-computed list of atom pairs for which to calculate RDFs or automatically determine all unique pairs present in the system. Similarly, neighbor lists can be provided or computed internally.

## Key Components

- **`MODULE cmpt_rdf`**: The main Fortran module containing the RDF computation logic.
- **`SUBROUTINE COMPUTE_RDF(H, P, rdf_maxR, rdf_dr, NeighList, aentries, pairs, rdf_sys)`**: This is the core subroutine that performs the RDF calculation.
    - It initializes parameters, including the number of radial bins (`rdf_Nsteps`) based on the maximum radius (`rdf_maxR`) and bin width (`rdf_dr`).
    - It determines the extent of periodic image searching needed based on `rdf_maxR` and the simulation cell vectors (`H`).
    - If not provided, it identifies the different atomic species and their counts (`aentries`) using `FIND_NSP`, and constructs a list of unique atomic pairs (`pairs`) for which partial RDFs will be calculated.
    - If a neighbor list (`NeighList`) is not provided, it constructs one using `NEIGHBOR_LIST`, ensuring neighbors up to `rdf_maxR + 1.1*rdf_dr` are found.
    - The main computation loop iterates over each atom `i` in the input system `P`. This loop is parallelized using OpenMP.
        - For each atom `i`, it considers contributions from its periodic self-images.
        - It then iterates through the neighbors `id` of atom `i` (obtained from `NeighList`).
        - For each pair of atoms (atom `i` and neighbor `id`, or atom `i` and its periodic image), it calculates the distance.
        - The distance is then used to determine which radial bin (shell/skin `n`) the pair contributes to.
        - A raw count for the specific atom pair type (e.g., A-B) and the corresponding radial bin `n` in the `rdf_sys` array is incremented.
    - After processing all atoms and their neighbors, the raw counts in `rdf_sys` are normalized. For each pair type and each radial bin `j`:
        - The volume of the spherical shell (`Vskin`) is calculated.
        - A normalization factor is computed: `(N_species1 * N_species2 * Vskin) / V_system`, where `N_species1` and `N_species2` are the total counts of the two species forming the pair, and `V_system` is the total volume of the simulation cell.
        - The raw count `rdf_sys(pair_type, j)` is divided by this normalization factor.
    - The final, normalized RDF data is stored in the output array `rdf_sys`.

## Important Variables/Constants

*   **Input Arguments:**
    - **`H (REAL(dp), DIMENSION(3,3))`**: The 3x3 matrix of supercell basis vectors.
    - **`P (REAL(dp), DIMENSION(:,:), ALLOCATABLE)`**: An allocatable array containing atomic data, where `P(i,1:3)` are the coordinates of atom `i` and `P(i,4)` is its atomic type/number.
    - **`rdf_maxR (REAL(dp))`**: The maximum radius (distance) up to which the RDF is to be computed.
    - **`rdf_dr (REAL(dp))`**: The width (thickness) of each radial bin or "skin" used for histogramming distances.

*   **Input/Output Arguments (can be pre-calculated and passed in, or will be calculated and returned):**
    - **`NeighList (INTEGER, DIMENSION(:,:), ALLOCATABLE)`**: Neighbor list for all atoms. If not allocated or undersized, it will be computed.
    - **`aentries (REAL(dp), DIMENSION(:,:), ALLOCATABLE)`**: Information about atomic species present. `aentries(k,1)` is the atomic number of species `k`, and `aentries(k,2)` is its count. If not allocated, it will be computed.
    - **`pairs (REAL(dp), DIMENSION(:,:), ALLOCATABLE)`**: List of unique atomic species pairs. `pairs(m,1)` and `pairs(m,2)` are the atomic numbers of the two species in pair `m`. If not allocated, it will be computed.

*   **Output Arguments:**
    - **`rdf_sys (REAL(dp), DIMENSION(:,:), ALLOCATABLE)`**: The primary output. A 2D array where `rdf_sys(m, n)` stores the computed RDF value for the `m`-th atomic pair type at the `n`-th radial distance bin. The number of rows is `Npairs`, and the number of columns is `rdf_Nsteps`.

*   **Key Internal Variables:**
    - **`rdf_Nsteps (INTEGER)`**: The number of radial bins (skins) determined by `rdf_maxR / rdf_dr`.
    - **`Nspecies (INTEGER)`**: Number of distinct atomic species found in the system.
    - **`Npairs (INTEGER)`**: Number of unique pairs of atomic species.
    - **`Vsystem (REAL(dp))`**: Total volume of the simulation cell.
    - **`Vskin (REAL(dp))`**: Volume of an individual spherical shell/skin.
    - **`rdf_norm (REAL(dp))`**: Normalization factor applied to the raw pair counts.

## Usage Examples

This subroutine is typically called by other parts of the Atomsk program when a user requests an RDF calculation. A direct Fortran call would involve preparing the atomic system data and RDF parameters:

```fortran
PROGRAM test_compute_rdf
  USE cmpt_rdf
  USE atoms_module ! Assume P (atom positions, types) and H (cell vectors) are defined
  IMPLICIT NONE

  REAL(dp), DIMENSION(3,3) :: H
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: P
  REAL(dp) :: max_radius, delta_r
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: neighbor_list
  REAL(dp), DIMENSION(:,:), ALLOCATABLE :: species_entries, pair_types, rdf_data

  ! ... Initialize P (e.g., N atoms, P(N,4)) and H ...
  ! ... Set max_radius and delta_r for RDF calculation ...
  ! e.g., max_radius = 10.0 ! Angstroms
  !       delta_r = 0.05  ! Angstroms

  ! NeighList, species_entries, pair_types can be left unallocated
  ! if COMPUTE_RDF is to determine them.

  CALL COMPUTE_RDF(H, P, max_radius, delta_r, neighbor_list, species_entries, pair_types, rdf_data)

  IF (ALLOCATED(rdf_data)) THEN
    PRINT *, "RDF calculation successful."
    ! rdf_data(i,j) contains g(r) for pair type i at radius (j-1)*delta_r
    ! species_entries tells which species are present
    ! pair_types(i,1) and pair_types(i,2) give the atomic numbers for the i-th pair type

    ! Example: Print RDF for the first pair type
    ! DO j = 1, SIZE(rdf_data, 2)
    !   PRINT *, (j-1)*delta_r, rdf_data(1,j)
    ! END DO

    ! ... Deallocate arrays ...
    DEALLOCATE(P, neighbor_list, species_entries, pair_types, rdf_data)
  ELSE
    PRINT *, "RDF calculation failed or produced no data."
  END IF

END PROGRAM test_compute_rdf
```

## Dependencies and Interactions

- **`atoms`**: Likely used for definitions related to atomic data structures (though not explicitly shown in the `USE` effects within this subroutine).
- **`comv`**: For global variables, particularly `verbosity` to control message output.
- **`constants`**: Provides mathematical constants like `pi`.
- **`messages`**: Uses the `ATOMSK_MSG` subroutine for standardized console output (informational messages, progress).
- **`neighbors`**: Depends on the `NEIGHBOR_LIST` subroutine to find neighbors if a list is not provided.
- **`files`**: Its usage is not directly apparent in `COMPUTE_RDF` but might be used by called subroutines or for related file operations elsewhere.
- **`subroutines`**:
    - `FIND_NSP`: To identify and count different atomic species in the system if `aentries` is not supplied.
    - `VOLUME_PARA`: To calculate the volume of the simulation cell (`Vsystem`).
    - `VECLENGTH`: To calculate distances between atoms and periodic images.
- **OpenMP**: The main loop for accumulating pair counts (`DO i=1,SIZE(P,1)`) is parallelized using OpenMP directives (`!$OMP PARALLEL DO`) to speed up computation on multi-core processors. The `rdf_sys` array uses `REDUCTION(+:rdf_sys)` for safe parallel updates.
