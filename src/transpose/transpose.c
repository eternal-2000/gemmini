#include "transpose/transpose.h"

/*
  TODO: Currently performs well for small matrices, and performs very well for large powers of 2.
 */

void transpose(int m, int n, const double* restrict A, int ldA, double* restrict B, int ldB){
  if (m <= 16 && n <= 16){ // Base cases
    if (m == 16 && n == 16){ // Kernel assumes aligned data
      transpose16x16(A, ldA, B, ldB);
    } else{
      for (int i = 0; i < m; ++i){
	for (int j = 0; j < n; ++j){
	  B[j + i * ldB] = A[i + j * ldA];
	}
      } 
    }
    return;
  }

  /*
    Otherwise, partition the matrix into a 2 x 2 block and recurse on each of the blocks
    Partitions are chosen by taking the largest submatrices which have dimensions equal to exact powers of 2,
    then recursing.    
   */

  int split_m = nextbit(m);
  int split_n = nextbit(n);

  transpose(split_m, split_n, A, ldA, B, ldB); 
  transpose(m - split_m, split_n, &A[split_m], ldA, &B[split_m * ldB], ldB); 
  transpose(split_m, n - split_n, &A[split_n * ldA], ldA, &B[split_n], ldB); 
  transpose(m - split_m, n - split_n, &A[split_m + split_n * ldA], ldA, &B[split_n + split_m * ldB], ldB);
}
