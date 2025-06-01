# `cbindings_runpolycrys.f90`

## Overview

The `cbindings_runpolycrys.f90` module serves as a C-language interface to Atomsk's polycrystalline structure generation capabilities, specifically wrapping the `POLYCRYS` Fortran subroutine. This allows external C programs to utilize Atomsk's algorithms for creating polycrystalline atomic configurations. The C program provides paths to a unit cell file (acting as a seed for grains) and a file containing parameters for Voronoi tessellation (defining grain morphology). Additional Atomsk options can also be passed. The module then invokes the Fortran backend to generate the structure, returning the atomic coordinates and final supercell vectors to the C caller. Memory for the atomic data is managed by Fortran and exposed via a C pointer, with a corresponding deallocation function.

## Key Components

- **`MODULE cbindings_runpolycrys`**: The Fortran module that houses the C binding interface.
- **`SUBROUTINE RUN_POLYCRYS(...) BIND(C)`**: This is the main C-callable subroutine. It receives input from the C side, including file paths for the unit cell (seed) and Voronoi parameters, an array of option strings, and an initial guess for the supercell basis vectors. These inputs are converted to Fortran-compatible types. The subroutine then calls the `POLYCRYS` routine from the `mode_polycrystal` module. After `POLYCRYS` completes, the generated atomic coordinates and the (potentially modified) supercell vectors are made available to the C caller. The number of atoms generated is also returned.
- **`SUBROUTINE DEALLOCATE_POLYCRYS(POUT) BIND(C)`**: A C-callable utility subroutine for memory management. It deallocates the Fortran memory array (`PPOINT`) that holds the atomic coordinates, which was allocated during the `RUN_POLYCRYS` call and made accessible to C through the `POUT` pointer. This function is crucial for preventing memory leaks in the calling C application.
- **`PPOINT (REAL(dp), POINTER :: PPOINT(:,:))`**: A module-level Fortran pointer used to store the generated atomic positions and their types. This allows `DEALLOCATE_POLYCRYS` to access and deallocate the memory after `RUN_POLYCRYS` has finished.

## Important Variables/Constants

These variables define the C interface for `RUN_POLYCRYS`.

*   **Input C variables (passed by the C caller):**
    - **`c_ucfile (CHARACTER(KIND=C_CHAR), DIMENSION(UCFILESIZE))`**: A C character array (string) representing the file path to the unit cell or seed structure.
    - **`UCFILESIZE (INTEGER(C_INT))`**: The length of the `c_ucfile` string.
    - **`c_vfile (CHARACTER(KIND=C_CHAR), DIMENSION(VFILESIZE))`**: A C character array (string) representing the file path to the Voronoi parameter file. This file typically defines the positions and number of Voronoi seeds to generate grains.
    - **`VFILESIZE (INTEGER(C_INT))`**: The length of the `c_vfile` string.
    - **`NUMOPTIONS (INTEGER(C_INT))`**: The number of option strings being passed.
    - **`c_options_array (CHARACTER(KIND=C_CHAR), DIMENSION(*))`**: A C character array containing concatenated option strings, each 128 characters long (space-padded), to be applied during or after polycrystal generation.
    - **`c_H (REAL(C_DOUBLE), DIMENSION(3,3))`**: A 3x3 C array representing the initial guess or desired supercell basis vectors. These may be adjusted by the `POLYCRYS` routine.

*   **Output C variables (modified by `RUN_POLYCRYS`):**
    - **`NUMATOMS (INTEGER(C_INT))`**: An integer that will be populated with the total number of atoms in the generated polycrystalline system.
    - **`c_POUT (TYPE(C_PTR), TARGET)`**: A C pointer that, upon successful return, will point to a Fortran-allocated memory block. This block contains the atomic data as a 2D array of size `NUMATOMS` x 4 (x, y, z, atomic_number).
    - **`c_H (REAL(C_DOUBLE), DIMENSION(3,3))`**: The 3x3 C array for the supercell basis vectors, updated to reflect the actual dimensions of the generated polycrystalline structure.

*   **Key Internal Fortran Variables (within `RUN_POLYCRYS`):**
    - **`ucfile (CHARACTER(LEN=UCFILESIZE))`**: Fortran variable for the unit cell filename.
    - **`vfile (CHARACTER(LEN=VFILESIZE))`**: Fortran variable for the Voronoi parameter filename.
    - **`options_array (CHARACTER(LEN=128), ALLOCATABLE, DIMENSION(:))`**: Fortran array for options.
    - **`H (REAL(dp), DIMENSION(3,3))`**: Fortran array for the supercell vectors.
    - **`P (REAL(dp), ALLOCATABLE, DIMENSION(:,:))`**: Fortran allocatable array where results from `POLYCRYS` are initially stored.
    - **`wof (LOGICAL)`**: Write output file flag, hardcoded to `.FALSE.` in this binding, meaning output is handled via pointers, not direct file writing from `POLYCRYS`.

## Usage Examples

This module is designed to be invoked from C. The C program would set up the file paths, any options, and initial cell parameters, then call `RUN_POLYCRYS`. After using the generated atomic data (accessed via `c_POUT`) and cell vectors, the C program *must* call `DEALLOCATE_POLYCRYS` to release the Fortran-allocated memory.

```c
// Conceptual C code example:

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Assumed C declarations for the Fortran routines:
// extern void run_polycrys_(char* c_ucfile, int* ucfilesize, char* c_vfile, int* vfilesize,
//                           int* numoptions, char* c_options_array,
//                           int* numatoms, void** c_pout, double* c_h,
//                           int ucfile_len, int vfile_len, int options_len); // Actual string lengths
// extern void deallocate_polycrys_(void** c_pout);

int main() {
    char unitcell_filename[] = "seed_unitcell.xsf";
    int uc_len = strlen(unitcell_filename);
    char voronoi_filename[] = "voronoi_params.txt";
    int vor_len = strlen(voronoi_filename);

    int num_opts = 0; // No additional options for simplicity
    char* opts_arr = NULL; // No options array

    int num_generated_atoms;
    double* atom_data_ptr = NULL;
    double cell_vectors[3][3] = {{100.0, 0.0, 0.0}, {0.0, 100.0, 0.0}, {0.0, 0.0, 100.0}}; // Initial large box

    // Call the Fortran subroutine for polycrystal generation
    // Note: String length passing varies by compiler/system for Fortran C bindings.
    // Sometimes lengths are passed as hidden arguments.
    run_polycrys_(unitcell_filename, &uc_len, voronoi_filename, &vor_len,
                  &num_opts, opts_arr,
                  &num_generated_atoms, (void**)&atom_data_ptr,
                  (double*)cell_vectors
                  // Depending on compiler, string lengths might be passed here explicitly
                  // uc_len, vor_len, 0 (for options_array length if NULL)
                  );

    if (atom_data_ptr != NULL) {
        printf("Polycrystal generated with %d atoms.\n", num_generated_atoms);
        // Access atomic data through atom_data_ptr
        // e.g., atom_data_ptr[0] is x-coord of first atom, etc.
        // Access updated cell_vectors.

        // IMPORTANT: Deallocate the memory
        deallocate_polycrys_((void**)&atom_data_ptr);
        printf("Memory for polycrystal data deallocated.\n");
    } else {
        printf("Failed to generate polycrystal or retrieve data.\n");
    }

    return 0;
}
```

## Dependencies and Interactions

- **`ISO_C_BINDING`**: Essential Fortran intrinsic module for defining C-compatible interfaces, data types (`C_CHAR`, `C_INT`, `C_DOUBLE`, `C_PTR`), and the `BIND(C)` attribute.
- **`mode_polycrystal` (External Fortran Module)**: This is a primary dependency, as it provides the `POLYCRYS` subroutine that performs the actual polycrystalline structure generation. `cbindings_runpolycrys` wraps this functionality for C.
- **`writeout` (External Fortran Module)**: The `USE writeout` statement is included. However, the flag `wof` (write output file) is set to `.FALSE.` within `RUN_POLYCRYS`, indicating that direct file output by the `POLYCRYS` routine is suppressed in this C-binding context. Data is returned to C via pointers.
- **File System**: The module interacts with the file system by reading the unit cell (seed) file and the Voronoi parameter file whose paths are provided by the C caller.
- **C Compiler & Linker**: For utilization, the compiled Fortran object code of this module must be linked with C object code. A C header file declaring the C function signatures for `RUN_POLYCRYS` and `DEALLOCATE_POLYCRYS` would typically be needed for the C compilation stage.
- **Memory Management**: `RUN_POLYCRYS` allocates memory on the Fortran heap for the atomic data (`PPOINT`) and returns a C pointer (`c_POUT`) to it. The C program is responsible for calling `DEALLOCATE_POLYCRYS` to free this memory, preventing leaks.
- **String Handling**: C strings (file paths, options) are passed to Fortran. The `TRANSFER` intrinsic is used for conversion. The Fortran code expects fixed-length character arrays based on `UCFILESIZE` and `VFILESIZE` or 128 characters for options. The C side must ensure these lengths are correctly communicated or handled (e.g., by padding or ensuring the Fortran side uses the provided lengths correctly).
