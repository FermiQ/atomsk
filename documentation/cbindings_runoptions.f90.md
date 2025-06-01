# `cbindings_runoptions.f90`

## Overview

The `cbindings_runoptions.f90` module provides a C-language interface to Atomsk's `OPTIONS_AFF` subroutine, which is responsible for applying a wide range of modifications and transformations to atomic systems. This module allows a C program to define an atomic structure (including atomic positions, core-shell information, auxiliary properties, unit cell and supercell vectors, crystallographic orientation, atom selection status, and elastic stiffness tensor) and a list of Atomsk options. The module then passes this data to the Fortran backend where the options are applied, and the modified atomic system is made available back to the C caller, largely through pointers to Fortran-managed memory. It also includes a crucial deallocation routine to manage this memory.

## Key Components

- **`MODULE cbindings_runoptions`**: The main Fortran module that encapsulates the C binding interface.
- **`SUBROUTINE RUN_OPTIONS(...) BIND(C)`**: This is the primary C-callable subroutine. It takes numerous arguments from the C side, representing the complete state of an atomic system and a list of options to be applied. It converts these C-style inputs into Fortran-compatible data structures, calls the `OPTIONS_AFF` subroutine (from the `options` module in Atomsk) to perform the actual operations, and then prepares the output data (modified atomic coordinates, shells, auxiliary properties, selection status, and cell parameters) for the C caller, typically by updating C pointers to point to Fortran-allocated memory or by modifying C arrays in place.
- **`SUBROUTINE DEALLOCATE_OPTIONS(POUT, SOUT, AUXOUT, SELECTOUT) BIND(C)`**: A C-callable subroutine essential for memory management. It deallocates the Fortran memory arrays (`PPOINT`, `SPOINT`, `AUXPOINT`, `SELECTPOINT`) that were allocated by `RUN_OPTIONS` and made accessible to C via pointers. This must be called by the C program to prevent memory leaks.
- **`PPOINT (REAL(dp), POINTER :: PPOINT(:,:))`**: Module-level Fortran pointer for atomic coordinates and types.
- **`SPOINT (REAL(dp), POINTER :: SPOINT(:,:))`**: Module-level Fortran pointer for shell coordinates and types.
- **`AUXPOINT (REAL(dp), POINTER :: AUXPOINT(:,:))`**: Module-level Fortran pointer for auxiliary atomic properties.
- **`SELECTPOINT (LOGICAL, POINTER :: SELECTPOINT(:))`**: Module-level Fortran pointer for the atom selection status array.

## Important Variables/Constants

These variables define the interface for the `RUN_OPTIONS` subroutine, detailing the data passed from C to Fortran and back.

*   **Input C variables (passed by the C caller):**
    - **`NUMOPTIONS (INTEGER(C_INT))`**: Number of option strings to be applied.
    - **`c_options_array (CHARACTER(KIND=C_CHAR), DIMENSION(*))`**: A C character array containing concatenated option strings. Each option string is 128 characters long, space-padded.
    - **`NUMATOMS (INTEGER(C_INT))`**: Initial number of atoms in the system.
    - **`c_PIN (REAL(C_DOUBLE), DIMENSION(NUMATOMS,4))`**: A C array containing initial atomic data [x, y, z, atomic_number] for each atom.
    - **`NUMSHELLS (INTEGER(C_INT))`**: Initial number of shells (for core-shell models).
    - **`c_SIN (REAL(C_DOUBLE), DIMENSION(NUMSHELLS,4))`**: A C array containing initial shell data [x, y, z, shell_type/link_to_core] for each shell.
    - **`NUMAUXNAMES (INTEGER(C_INT))`**: Number of auxiliary property names defined.
    - **`c_AUXNAMES (CHARACTER(KIND=C_CHAR), DIMENSION(*))`**: A C character array containing concatenated auxiliary property names. Each name is 128 characters long, space-padded.
    - **`c_AUXIN (REAL(C_DOUBLE), DIMENSION(NUMATOMS,NUMAUXNAMES))`**: A C array containing initial values for auxiliary properties for each atom.
    - **`c_Huc (REAL(C_DOUBLE), DIMENSION(3,3))`**: 3x3 C array for the unit cell basis vectors.
    - **`c_H (REAL(C_DOUBLE), DIMENSION(3,3))`**: 3x3 C array for the supercell basis vectors.
    - **`c_ORIENT (REAL(C_DOUBLE), DIMENSION(3,3))`**: 3x3 C array for the crystallographic orientation matrix.
    - **`c_SELECTIN (INTEGER(C_INT), DIMENSION(NUMATOMS))`**: A C integer array indicating initial atom selection status (1 for selected, 0 for not selected).
    - **`c_C_tensor (REAL(C_DOUBLE), DIMENSION(9,9))`**: 9x9 C array for the stiffness tensor.

*   **Output C variables (modified by `RUN_OPTIONS`):**
    - **`c_POUT (TYPE(C_PTR), TARGET)`**: A C pointer that will point to Fortran-allocated memory containing the modified atomic data ([x,y,z,atomic_number]). The number of atoms might change, so `NUMATOMS` (passed by reference from C, though not explicitly shown as `INTENT(OUT)` for the C binding here but implicitly modified before `PPOINT` is allocated based on `SIZE(P,1)`) should be updated/checked by the C caller after the call.
    - **`c_SOUT (TYPE(C_PTR), TARGET)`**: A C pointer to Fortran-allocated memory for modified shell data. `NUMSHELLS` might also change.
    - **`c_AUXOUT (TYPE(C_PTR), TARGET)`**: A C pointer to Fortran-allocated memory for modified auxiliary properties.
    - **`c_SELECTOUT (TYPE(C_PTR), TARGET)`**: A C pointer to Fortran-allocated memory for the modified atom selection status (logical array).
    - **`c_Huc, c_H, c_ORIENT, c_C_tensor`**: These 3x3 (or 9x9) C arrays are modified in-place with the new cell vectors, orientation, and stiffness tensor.
    - **Note**: `NUMATOMS` and `NUMSHELLS` (passed as arguments to `RUN_OPTIONS`) are updated by `RUN_OPTIONS` to reflect changes due to the applied options before memory for `PPOINT`, `SPOINT` etc. is allocated. The C caller should use these updated values.

## Usage Examples

This module is intended to be called from C code. The C application would prepare all necessary input arrays and variables, invoke `RUN_OPTIONS`, and then process the results using the data pointed to by `c_POUT`, `c_SOUT`, `c_AUXOUT`, `c_SELECTOUT`, and the modified arrays `c_Huc`, `c_H`, etc. Crucially, `DEALLOCATE_OPTIONS` must be called with the output pointers to free the memory allocated by Fortran.

```c
// Conceptual C code example:
#include <stdio.h>
#include <stdlib.h> // For malloc/free if C side needs to manage its own copies
#include <string.h> // For string operations

// Assumed C declarations for the Fortran routines:
// extern void run_options_(int* numoptions, char* options_array,
//                          int* numatoms, double* pin, void** pout,
//                          int* numshells, double* sin, void** sout,
//                          int* numauxnames, char* auxnames, double* auxin, void** auxout,
//                          double* huc, double* h, double* orient,
//                          int* selectin, void** selectout, double* c_tensor,
//                          int options_len, int auxnames_len);
// extern void deallocate_options_(void** pout, void** sout, void** auxout, void** selectout);

int main() {
    int num_atoms = 2;
    double atoms_in[2][4] = {{0.0, 0.0, 0.0, 26.0}, {0.5, 0.5, 0.5, 26.0}}; // Fe atoms
    int selection_in[2] = {1, 1}; // All selected
    double h_matrix[3][3] = {{2.87,0,0},{0,2.87,0},{0,0,2.87}}; // BCC Fe cell

    int num_opts = 1;
    char opts[129]; // 128 chars for option + null
    snprintf(opts, sizeof(opts), "%-128s", "-fix CHO"); // Example option

    // Pointers for output data
    double* atoms_out_ptr = NULL;
    double* shells_out_ptr = NULL; // Assuming no shells for simplicity here
    double* aux_out_ptr = NULL;   // Assuming no aux properties for simplicity
    int* select_out_ptr = NULL;  // Using int* as C bool might differ from Fortran LOGICAL

    int num_shells_in = 0; // No shells
    int num_aux_names_in = 0; // No aux properties

    // Call the Fortran options routine
    // Note: String lengths are often passed implicitly or as extra args by compilers
    run_options_(&num_opts, opts,
                 &num_atoms, (double*)atoms_in, (void**)&atoms_out_ptr,
                 &num_shells_in, NULL, (void**)&shells_out_ptr, // No shells
                 &num_aux_names_in, NULL, NULL, (void**)&aux_out_ptr, // No aux
                 (double*)h_matrix, (double*)h_matrix, (double*)h_matrix, // Using same H for Huc, H, Orient for simplicity
                 selection_in, (void**)&select_out_ptr, (double*)NULL /*C_tensor*/);

    printf("RUN_OPTIONS called. Updated number of atoms: %d\n", num_atoms);

    if (atoms_out_ptr) {
        // Access atoms_out_ptr, shells_out_ptr, etc.
        // Remember these point to Fortran-ordered memory.
        // The number of atoms might have changed.
    }

    // Deallocate Fortran-managed memory
    deallocate_options_((void**)&atoms_out_ptr, (void**)&shells_out_ptr,
                        (void**)&aux_out_ptr, (void**)&select_out_ptr);
    printf("Memory deallocated.\n");

    return 0;
}
```

## Dependencies and Interactions

- **`ISO_C_BINDING`**: This Fortran intrinsic module is fundamental for ensuring type compatibility and calling conventions between C and Fortran. It provides kinds for C types (`C_INT`, `C_DOUBLE`, `C_CHAR`, `C_PTR`) and the `BIND(C)` attribute.
- **`options` (External Fortran Module)**: This is a core dependency. The `cbindings_runoptions` module is essentially a C wrapper around the `OPTIONS_AFF` subroutine provided by the `options` module, which performs the actual application of transformations and modifications to the atomic system.
- **`writeout` (External Fortran Module)**: The `USE writeout` statement is present, but the binding itself does not directly perform file writing operations. Any such operations would be handled internally by `OPTIONS_AFF` if triggered by a specific option.
- **C Compiler & Linker**: To use this module, the Fortran source code must be compiled and linked with C code. A C header file (`.h`) defining the C prototypes for `RUN_OPTIONS` and `DEALLOCATE_OPTIONS` is typically required by the C compiler.
- **Memory Management**: This is a critical aspect. `RUN_OPTIONS` allocates memory on the Fortran side for the output arrays (`PPOINT`, `SPOINT`, `AUXPOINT`, `SELECTPOINT`) and provides C pointers to this memory. The C calling program *must* call `DEALLOCATE_OPTIONS` with these pointers to free the Fortran-allocated memory and prevent leaks.
- **Data Representation**: The `TRANSFER` intrinsic is used to copy data between C and Fortran representations. Care must be taken with array ordering (Fortran is column-major, C is row-major) if direct array manipulation is done across the boundary, though here full structures are passed and received. String data from C must be correctly padded to the fixed lengths expected by Fortran. The logical array `SELECT` in Fortran is returned as a C pointer, and its interpretation in C (e.g., as `int` or `bool`) needs care.
