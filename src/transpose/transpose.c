#include "transpose/transpose.h"
void transpose(int m, int n, double* restrict A, int ldA, double* restrict B, int ldB){
  if (m <= 16 && n <= 16){
    if (m == 16 && n == 16){
      transpose16x16(A, ldA, B, ldB); // Kernel assumes aligned data
    } else{
      for (int i = 0; i < m; ++i){
	for (int j = 0; j < n; ++j){
	  B[j + i * ldB] = A[i + j * ldA];
	}
      } 
    }
    return;
  }

  /**
    Partitions the matrix into a 2 x 2 transposed block and recurses on each of the blocks
    Partitions are chosen by taking the largest submatrices which have dimensions equal to exact powers of 2,
    then recursing.
  **/

  int split_m = nextbit(m);
  int split_n = nextbit(n);

  transpose(split_m, split_n, A, ldA, B, ldB); 
  transpose(m - split_m, split_n, &A[split_m], ldA, &B[split_m * ldB], ldB); 
  transpose(split_m, n - split_n, &A[split_n * ldA], ldA, &B[split_n], ldB); 
  transpose(m - split_m, n - split_n, &A[split_m + split_n * ldA], ldA, &B[split_n + split_m * ldB], ldB);
}
