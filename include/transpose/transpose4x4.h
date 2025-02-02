#ifndef TRANSPOSE4X4_H
#define TRANSPOSE4X4_H
#include<immintrin.h>

static inline void transpose4x4(const double* restrict A, const int ldA, double* restrict B, const int ldB){
  __m256d col0 = _mm256_load_pd(&A[0]);
  __m256d col1 = _mm256_load_pd(&A[ldA]);
  __m256d col2 = _mm256_load_pd(&A[2 * ldA]);
  __m256d col3 = _mm256_load_pd(&A[3 * ldA]);

  /* Manual implementation of _MM_TRANSPOSE4_PD if it existed:
     The following intrinsics will shuffle elements in columns of A around until col1, ..., col4 have elements
     matching the rows of A instead. 
   */
  
  __m256d temp010 = _mm256_unpacklo_pd(col0, col1);
  __m256d temp011 = _mm256_unpackhi_pd(col0, col1);
  __m256d temp230 = _mm256_unpacklo_pd(col2, col3);
  __m256d temp231 = _mm256_unpackhi_pd(col2, col3);

  col0 = _mm256_permute2f128_pd(temp010, temp230, 0x20);
  col1 = _mm256_permute2f128_pd(temp011, temp231, 0x20);
  col2 = _mm256_permute2f128_pd(temp010, temp230, 0x31);
  col3 = _mm256_permute2f128_pd(temp011, temp231, 0x31);

  /* End "_MM_TRANSPOSE4_PD" */
  
  _mm256_store_pd(&B[0], col0);
  _mm256_store_pd(&B[ldB], col1);
  _mm256_store_pd(&B[2 * ldB], col2);
  _mm256_store_pd(&B[3 * ldB], col3); 
}
#endif
