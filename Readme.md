## README ##
==================================================
This is an implementation of double-precision general matrix multiplication (dgemm), with an AVX2 microkernel,
meant for personal experimentation. 

This algorithm performs the update C += AB, where C is m x n, A is m x p, and B is p x n and entries are doubles.
This also correctly implements versions where A, B, or both are transposed.

==================================================
# General description #
==================================================

This follows the BLIS view of considering the GeMM operation as five loops around the microkernel, which is fixed at
compile time, and updates microtiles of C by a rank-1 update with micropanels of A and B. In the event that the 
dimensions of the microtile are not big enough to make it fit exactly into the microkernel, we use a temporary
full-sized buffer instead. 

Packing of A and B microtiles into contiguous memory is handled in the third and fourth loops around the microkernel,
and transposition is also handled here to minimise any cost.

Syntax to call test functions is ./<test-function> "X" "X" initial-size final-size increment repetitions, where "X"
should be "T" (transpose) or "N" (no transpose), initial-size and final-size are the dimensions of the first and last
square matrices to test, the increment is the number of dimensions to add each step, and repetitions is the number of 
times to repeat the multiplication to get performance data.

==================================================
# Performance #
==================================================

Currently performance is measured by taking the peak performance in a given number of runs (3 by default). Performance
can also be measured against the BLIS reference implementation. This assumes that BLIS is installed and located in
~/blis. Performance can be plotted by calling the Python script in the test directory.

Currently, only the fifth loop around the microkernel is parallelised. This is because I'm still playing around with
a proper implementation of parallelised inner loops as well. This still enables around 90-99 % of the performance of BLIS
for smaller (less than ~2000 x 2000) matrix sizes on my own CPU at present and should be improved further in a future update.

==================================================
# Cache parameter notes #
==================================================

The compile-time constants MC, NC, KC represent the sizes of the blocks into which the matrices are broken up in the 
cache, and MR, NR represent the size of the matrices to move into the registers. MR and NR also determine the size of
the microkernel to use, and this is done at compile time. 

Currently, a few microkernels are supported, but the default is set to 8 x 4 since this works effectively on my own CPU.
These parameters can and should be adjusted for different CPUs. 

A script to automatically adjust them to optimal values for a given CPU is on the to-do list.
