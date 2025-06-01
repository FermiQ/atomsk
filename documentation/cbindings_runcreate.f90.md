# `cbindings_runcreate.f90`

## Overview

The `cbindings_runcreate.f90` module serves as a bridge between C and Fortran code, specifically for Atomsk's crystal structure creation capabilities. It provides C-callable wrapper functions that interface with the underlying Fortran `CREATE_CELL` subroutine (located in the `mode_create` module). This allows external C programs to leverage Atomsk's ability to generate atomic configurations based on various parameters like lattice type, constants, atomic species, and orientation. The module handles the necessary data type conversions (e.g., C strings to Fortran characters, C pointers to Fortran pointers) and manages memory allocation for the generated atomic data, providing a mechanism for C code to safely access and then deallocate this data.

## Key Components

- **`MODULE cbindings_runcreate`**: The container for the C binding interface subroutines.
- **`SUBROUTINE RUN_CREATE(...) BIND(C)`**: This is the core C-callable subroutine. It accepts parameters from a C environment, such as lattice constants, crystal structure type, atomic species, Miller indices for orientation, and any additional processing options. It then converts these parameters into Fortran-compatible types, invokes the `CREATE_CELL` subroutine from the `mode_create` module to generate the atomic system, and finally, makes the resulting atomic coordinates and cell vectors available to the C caller through C pointers.
- **`SUBROUTINE DEALLOCATE_CREATE(POUT) BIND(C)`**: A C-callable utility subroutine. Its purpose is to deallocate the Fortran-managed memory (specifically the `PPOINT` array) that was allocated during a call to `RUN_CREATE` and pointed to by the C pointer `POUT`. This is essential for preventing memory leaks in the calling C application.
- **`PPOINT (REAL(dp), POINTER :: PPOINT(:,:))`**: A module-level Fortran pointer. It is used to hold the generated atomic data (coordinates and atomic numbers) after the `CREATE_CELL` call. Making it a module-level pointer allows `DEALLOCATE_CREATE` to access and deallocate it, as the original `P` array in `RUN_CREATE` is local to that subroutine.

## Important Variables/Constants

These variables are primarily relevant within the context of the `RUN_CREATE` subroutine, representing the interface with the C caller.

*   **Input C variables (passed by the C caller):**
    - **`c_create_a0 (REAL(C_DOUBLE), DIMENSION(3))`**: An array containing the three lattice constants (a, b, c) in Angstroms.
    - **`c_create_struc (CHARACTER(KIND=C_CHAR), DIMENSION(10))`**: A C string (character array) specifying the crystal structure type (e.g., "fcc", "bcc"). Must be space-padded to 10 characters.
    - **`c_create_species (CHARACTER(KIND=C_CHAR), DIMENSION(40))`**: A C string containing the chemical symbols of atomic species for the unit cell. Each species symbol should occupy two characters, space-padded if necessary (e.g., "Ni  Fe  "), allowing for up to 20 species.
    - **`c_NT_mn (INTEGER(C_INT), DIMENSION(2))`**: An integer array holding the (n,m) chiral indices for creating nanotubes. If not applicable, these are typically zero.
    - **`c_create_Miller (CHARACTER(KIND=C_CHAR), DIMENSION(96))`**: A C string defining the Miller indices for the crystallographic orientation of the X, Y, and Z axes. Each set of Miller indices (e.g., "100") occupies 32 characters, space-padded.
    - **`NUMOPTIONS (INTEGER(C_INT))`**: An integer indicating the number of additional processing options being passed.
    - **`c_options_array (CHARACTER(KIND=C_CHAR), DIMENSION(*))`**: A C string (character array) containing a concatenated list of options to be applied after cell creation (e.g., "-duplicate", "-perturb"). Each option string is expected to be 128 characters long, space-padded.
    - **`c_H (REAL(C_DOUBLE), DIMENSION(3,3))`**: A 3x3 C array representing the initial cell vectors (basis vectors) of the supercell. This can often be initialized to zeros if `CREATE_CELL` is to determine them.

*   **Output C variables (modified by `RUN_CREATE` and returned to the C caller):**
    - **`NUMATOMS (INTEGER(C_INT))`**: An integer that will be populated with the total number of atoms generated in the system.
    - **`c_POUT (TYPE(C_PTR), TARGET)`**: A C pointer that, upon return, will point to a contiguous block of memory. This memory holds the atomic data as a 2D array of size `NUMATOMS` x 4, where each row contains [x, y, z, atomic_number].
    - **`c_H (REAL(C_DOUBLE), DIMENSION(3,3))`**: The 3x3 C array representing the final cell vectors of the generated supercell, as determined or modified by `CREATE_CELL`.

*   **Key Internal Fortran Variables (within `RUN_CREATE`):**
    - **`create_a0, create_struc, create_species, NT_mn, create_Miller`**: Fortran variables of appropriate types that receive the data from their C counterparts after conversion.
    - **`options_array (CHARACTER(LEN=128), ALLOCATABLE, DIMENSION(:))`**: Fortran array to store the parsed options.
    - **`P (REAL(dp), ALLOCATABLE, DIMENSION(:,:))`**: A local Fortran allocatable array where the results from `CREATE_CELL` (atomic positions and types) are initially stored.
    - **`H (REAL(dp), DIMENSION(3,3))`**: Fortran array for cell vectors.
    - **`wof (LOGICAL)`**: A flag indicating whether `CREATE_CELL` should write output files. It is hardcoded to `.FALSE.` in this binding, as data is returned via pointers.

## Usage Examples

The intended use of this module is from a C program. The C code would declare variables matching the interface of `RUN_CREATE`, populate them with the desired parameters for crystal generation, and then call `RUN_CREATE`. After processing the returned atomic data, the C code *must* call `DEALLOCATE_CREATE` with the pointer received in `c_POUT` to free the memory allocated by Fortran.

```c
// Conceptual C code example demonstrating a call to RUN_CREATE and DEALLOCATE_CREATE.
// A proper C header file declaring these Fortran functions would be needed.

#include <stdio.h>
#include <string.h> // For strcpy, strncat, etc.

// Assumed declarations from a C header file for the Fortran routines:
// extern void run_create_(double* c_create_a0, char* c_create_struc, char* c_create_species,
//                         int* c_NT_mn, char* c_create_Miller, int* NUMOPTIONS,
//                         char* c_options_array, int* NUMATOMS, void** c_POUT,
//                         double* c_H, int struc_len, int species_len,
//                         int miller_len, int options_len);
// extern void deallocate_create_(void** c_POUT);

int main() {
    double lattice_constants[3] = {4.05, 4.05, 4.05}; // For Al
    char structure[11] = "fcc       "; // 10 chars + null terminator for C string
    char species[41] = "Al                                      "; // 40 chars + null
    int nt_mn_indices[2] = {0, 0}; // Not a nanotube
    // Miller indices for X, Y, Z: e.g., X=[100], Y=[010], Z=[001]
    // Each index string (32 chars) + spaces if shorter + concatenation
    char miller_str[97]; // 96 chars + null
    snprintf(miller_str, sizeof(miller_str), "%-32s%-32s%-32s", "100", "010", "001");

    int num_options = 1;
    char options[129]; // 128 chars + null
    snprintf(options, sizeof(options), "%-128s", "-duplicate 2 2 2");

    int num_atoms_generated;
    double* atom_data_pointer = NULL; // Will point to the atom data array
    double cell_matrix[3][3] = {{0.0}}; // Initial cell matrix, can be zero

    // Call the Fortran subroutine (mangled name might vary by compiler)
    run_create_(lattice_constants, structure, species,
                nt_mn_indices, miller_str, &num_options,
                options, &num_atoms_generated, (void**)&atom_data_pointer,
                (double*)cell_matrix
                // Fortran string lengths often passed as hidden arguments
                // strlen(structure), strlen(species), strlen(miller_str), strlen(options)
                );

    if (atom_data_pointer != NULL) {
        printf("Successfully created %d atoms.\n", num_atoms_generated);
        // Example: Print coordinates of the first atom
        // Data is [x1,y1,z1,type1, x2,y2,z2,type2, ...]
        // printf("Atom 1: x=%.3f, y=%.3f, z=%.3f, type=%.0f\n",
        //        atom_data_pointer[0], atom_data_pointer[1],
        //        atom_data_pointer[2], atom_data_pointer[3]);

        // IMPORTANT: Deallocate the memory provided by Fortran
        deallocate_create_((void**)&atom_data_pointer);
        printf("Memory deallocated.\n");
    } else {
        printf("Failed to create atoms or retrieve data.\n");
    }

    return 0;
}
```

## Dependencies and Interactions

- **`ISO_C_BINDING`**: This Fortran intrinsic module is crucial. It provides tools for interoperability with C, including C-compatible data types (like `C_DOUBLE`, `C_CHAR`, `C_INT`, `C_PTR`) and the `BIND(C)` attribute for subroutines.
- **`mode_create` (External Fortran Module)**: This module is a core dependency, as it contains the actual `CREATE_CELL` Fortran subroutine that performs the crystal structure generation. `cbindings_runcreate` acts as a C-friendly wrapper around `CREATE_CELL`.
- **`writeout` (External Fortran Module)**: Although `USE writeout` is present in the module, its direct file-writing capabilities are intentionally bypassed within `RUN_CREATE` by setting the logical flag `wof` (write output file) to `.FALSE.`. Data is meant to be returned to the C caller via pointers.
- **C Compiler & Linker**: For this module to be useful, the Fortran code must be compiled (typically into an object file or library) and then linked with C code. A C header file (`.h`) that declares the C prototypes for `RUN_CREATE` and `DEALLOCATE_CREATE` is usually necessary for the C compiler to correctly understand how to call these Fortran routines.
- **Memory Management**: A critical interaction point. `RUN_CREATE` allocates memory on the Fortran side (for `PPOINT`, which holds the atomic data) and provides a C pointer (`c_POUT`) to this memory. The calling C program is then responsible for explicitly calling `DEALLOCATE_CREATE` with this pointer to free the memory, preventing memory leaks. The `TARGET` attribute for `c_POUT` and the `POINTER` attribute for `PPOINT` are fundamental to this mechanism.
- **Data Transfer and String Handling**: The `TRANSFER` intrinsic function is used extensively to copy data between C variables and Fortran variables, ensuring type compatibility. Handling character strings requires careful management of lengths and padding, as C strings are null-terminated while Fortran character variables have fixed lengths. The comments in the Fortran code highlight the expected padding for string inputs.
