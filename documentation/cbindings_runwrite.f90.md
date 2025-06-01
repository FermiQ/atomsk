# `cbindings_runwrite.f90`

## Overview

The `cbindings_runwrite.f90` module acts as a C-language interface to Atomsk's file writing capabilities, specifically by wrapping the `WRITE_AFF` Fortran subroutine from the `writeout` module. This allows an external C program to take a fully defined atomic system—including atomic coordinates, core-shell data (if any), auxiliary atomic properties, and supercell vectors—and instruct Atomsk to write this system to one or more output files in specified Atomsk-supported formats. The C program provides all necessary data and parameters for the write operation. Unlike other C-binding modules that return data via pointers, this module's primary output is the creation of files on the disk.

## Key Components

- **`MODULE cbindings_runwrite`**: The Fortran module that contains the C binding interface.
- **`SUBROUTINE RUN_WRITE(...) BIND(C)`**: This is the sole C-callable subroutine in this module. It accepts all data describing an atomic system (number of atoms, atomic coordinates and types, number of shells, shell coordinates and types, auxiliary property names and their per-atom values, and supercell basis vectors). Additionally, it takes a filename prefix, the length of this prefix, a list of desired output file formats (as 5-character strings), and the number of such formats. The subroutine converts these C inputs into Fortran-compatible data structures and then calls the `WRITE_AFF` subroutine to perform the file writing operations. It does not return any data to the C caller directly; its effect is the creation of output files.

## Important Variables/Constants

These variables define the C interface for the `RUN_WRITE` subroutine.

*   **Input C variables (passed by the C caller):**
    - **`NUMATOMS (INTEGER(C_INT))`**: The total number of atoms in the system.
    - **`c_P (REAL(C_DOUBLE), DIMENSION(NUMATOMS,4))`**: A C array containing the atomic data: [x, y, z, atomic_number] for each of the `NUMATOMS` atoms.
    - **`NUMSHELLS (INTEGER(C_INT))`**: The number of shells in the system (for core-shell models; can be 0).
    - **`c_S (REAL(C_DOUBLE), DIMENSION(NUMSHELLS,4))`**: A C array containing shell data: [x, y, z, shell_type/link_to_core_atom_index] for each of the `NUMSHELLS` shells.
    - **`NUMAUXNAMES (INTEGER(C_INT))`**: The number of auxiliary properties defined for the atoms (e.g., velocity, force).
    - **`c_AUXNAMES (CHARACTER(KIND=C_CHAR), DIMENSION(*))`**: A C character array containing concatenated names for the auxiliary properties. Each name is expected to be 128 characters long, space-padded.
    - **`c_AUX (REAL(C_DOUBLE), DIMENSION(NUMATOMS,NUMAUXNAMES))`**: A C array containing the values of auxiliary properties for each atom.
    - **`c_H (REAL(C_DOUBLE), DIMENSION(3,3))`**: A 3x3 C array representing the supercell basis vectors (lattice vectors H(1,:), H(2,:), H(3,:)).
    - **`c_prefix (CHARACTER(KIND=C_CHAR), DIMENSION(PREFIXSIZE))`**: A C character array (string) for the desired output filename prefix.
    - **`PREFIXSIZE (INTEGER(C_INT))`**: The actual length of the `c_prefix` string.
    - **`c_fileformats (CHARACTER(KIND=C_CHAR), DIMENSION(*))`**: A C character array containing concatenated 5-character strings specifying the desired output file formats (e.g., "xsf  cfg  lmp  ").
    - **`NUMFORMATS (INTEGER(C_INT))`**: The number of output file formats specified in `c_fileformats`.

*   **Output C variables:** None. The subroutine's effect is purely side-effects (writing files).

*   **Key Internal Fortran Variables (within `RUN_WRITE`):**
    - **`P, S, AUXNAMES, AUX, H`**: Fortran arrays that receive data from their C counterparts.
    - **`prefix (CHARACTER(LEN=PREFIXSIZE))`**: Fortran variable for the filename prefix.
    - **`fileformats (CHARACTER(LEN=5), ALLOCATABLE, DIMENSION(:))`**: Fortran array to store the list of output formats.
    - **`comment (CHARACTER(LEN=128), ALLOCATABLE, DIMENSION(:))`**: An empty comment array is allocated and passed to `WRITE_AFF`, as comments are not taken from C in this binding.

## Usage Examples

This module is intended to be invoked from C code. The C program must prepare all the atomic system data (atom positions, types, cell vectors, any shell or auxiliary data) and specify the desired output filename prefix and format(s). After calling `RUN_WRITE`, Atomsk will attempt to write the corresponding file(s).

```c
// Conceptual C code example:

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Assumed C declarations for the Fortran routines:
// extern void run_write_(int* numatoms, double* c_p, int* numshells, double* c_s,
//                        int* numauxnames, char* c_auxnames, double* c_aux,
//                        double* c_h, char* c_prefix, int* prefixsize,
//                        char* c_fileformats, int* numformats,
//                        int auxnames_len, int prefix_len, int fileformats_len);

int main() {
    int num_atoms = 2;
    // Atom data: x, y, z, type (e.g., atomic number)
    double atom_coords[2][4] = {
        {0.0, 0.0, 0.0, 26.0}, // Atom 1 (Fe)
        {1.435, 1.435, 1.435, 26.0}  // Atom 2 (Fe)
    };
    // Supercell vectors for a BCC unit cell of Fe (lattice const 2.87 A)
    double cell_vectors[3][3] = {
        {2.87, 0.0,  0.0},
        {0.0,  2.87, 0.0},
        {0.0,  0.0,  2.87}
    };

    int num_shells = 0; // No shells
    double* shell_coords = NULL;

    int num_aux_props = 0; // No auxiliary properties
    char* aux_prop_names = NULL;
    double* aux_prop_values = NULL;

    char file_prefix[] = "my_structure";
    int prefix_len = strlen(file_prefix);

    // Output formats: xsf and cfg. Each is 5 chars, space-padded.
    char output_formats[] = "xsf  cfg  ";
    int num_formats = 2;
    int formats_str_len = strlen(output_formats); // Should be NUMFORMATS * 5

    // Call the Fortran subroutine to write files
    // String length passing details can be compiler-dependent.
    run_write_(&num_atoms, (double*)atom_coords, &num_shells, shell_coords,
               &num_aux_props, aux_prop_names, aux_prop_values,
               (double*)cell_vectors, file_prefix, &prefix_len,
               output_formats, &num_formats
               // strlen(aux_prop_names) - if not NULL
               // strlen(file_prefix)
               // strlen(output_formats)
               );

    printf("RUN_WRITE called. Check for files like '%s.xsf' and '%s.cfg'.\n", file_prefix, file_prefix);

    return 0;
}
```

## Dependencies and Interactions

- **`ISO_C_BINDING`**: This Fortran intrinsic module is essential for C interoperability, providing C-compatible data types and the `BIND(C)` attribute.
- **`writeout` (External Fortran Module)**: This is the core dependency. The `cbindings_runwrite` module serves as a C wrapper for the `WRITE_AFF` subroutine, which is provided by the `writeout` module and handles the logic for writing various file formats.
- **File System**: The primary interaction of this module (via `WRITE_AFF`) is with the file system, as it creates and writes data to output files (e.g., `my_structure.xsf`, `my_structure.cfg`). The success of these operations depends on file system permissions and path validity.
- **C Compiler & Linker**: The compiled Fortran object code of this module must be linked with C object code. A C header file declaring the C function signature for `RUN_WRITE` would typically be required for the C compilation process.
- **Memory Management**: Unlike C bindings that return data via pointers, `cbindings_runwrite` does not allocate persistent memory that needs deallocation from the C side. All data is passed from C to Fortran, and Fortran uses it to perform I/O operations. Internal Fortran arrays are allocated and deallocated within the `RUN_WRITE` call or within `WRITE_AFF`.
- **Data Transfer and String Handling**: The `TRANSFER` intrinsic is used to copy data from C variables to Fortran variables. Fixed-length strings are expected by Fortran for `AUXNAMES` (128 chars each), `prefix` (length `PREFIXSIZE`), and `fileformats` (5 chars each). The C side must ensure strings are correctly formatted (e.g., space-padded) and lengths are accurately provided.
