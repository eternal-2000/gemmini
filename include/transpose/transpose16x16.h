#ifndef TRANSPOSE16X16_H
#define TRANSPOSE16X16_H
#include<immintrin.h>

static inline void transpose16x16(const double* restrict A, const int ldA, double* restrict B, const int ldB){
  for (int i = 0; i < 16; i += 4){
    for (int j = 0; j < 16; j += 4){
      __m256d col0 = _mm256_load_pd(&A[j + i * ldA]);
      __m256d col1 = _mm256_load_pd(&A[j + (i + 1) * ldA]);
      __m256d col2 = _mm256_load_pd(&A[j + (i + 2) * ldA]);
      __m256d col3 = _mm256_load_pd(&A[j + (i + 3) * ldA]);

      __m256d temp010 = _mm256_unpacklo_pd(col0, col1);
      __m256d temp011 = _mm256_unpackhi_pd(col0, col1);
      __m256d temp230 = _mm256_unpacklo_pd(col2, col3);
      __m256d temp231 = _mm256_unpackhi_pd(col2, col3);

      col0 = _mm256_permute2f128_pd(temp010, temp230, 0x20);
      col1 = _mm256_permute2f128_pd(temp011, temp231, 0x20);
      col2 = _mm256_permute2f128_pd(temp010, temp230, 0x31);
      col3 = _mm256_permute2f128_pd(temp011, temp231, 0x31);

      _mm256_store_pd(&B[i + j * ldB], col0);
      _mm256_store_pd(&B[i + (j + 1) * ldB], col1);
      _mm256_store_pd(&B[i + (j + 2) * ldB], col2);
      _mm256_store_pd(&B[i + (j + 3) * ldB], col3);
    }
  }
}
#endif
