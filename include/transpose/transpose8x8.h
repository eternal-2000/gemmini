#ifndef TRANSPOSE8X8_H
#define TRANSPOSE8X8_H
#include<immintrin.h>

static inline void transpose8x8(const double* restrict A, const int ldA, double* restrict B, const int ldB){
  for (int i = 0; i < 8; i += 4){
    __m256d col0 = _mm256_load_pd(&A[i * ldA]);
    __m256d col1 = _mm256_load_pd(&A[(i + 1) * ldA]);
    __m256d col2 = _mm256_load_pd(&A[(i + 2) * ldA]);
    __m256d col3 = _mm256_load_pd(&A[(i + 3) * ldA]);

    __m256d temp010 = _mm256_unpacklo_pd(col0, col1);
    __m256d temp011 = _mm256_unpackhi_pd(col0, col1);
    __m256d temp230 = _mm256_unpacklo_pd(col2, col3);
    __m256d temp231 = _mm256_unpackhi_pd(col2, col3);

    col0 = _mm256_permute2f128_pd(temp010, temp230, 0x20);
    col1 = _mm256_permute2f128_pd(temp011, temp231, 0x20);
    col2 = _mm256_permute2f128_pd(temp010, temp230, 0x31);
    col3 = _mm256_permute2f128_pd(temp011, temp231, 0x31);

    _mm256_store_pd(&B[i], col0);
    _mm256_store_pd(&B[i + 1], col1);
    _mm256_store_pd(&B[i + 2], col2);
    _mm256_store_pd(&B[i + 3], col3);
    
    col0 = _mm256_load_pd(&A[4 + i * ldA]);
    col1 = _mm256_load_pd(&A[4 + (i + 1) * ldA]);
    col2 = _mm256_load_pd(&A[4 + (i + 2) * ldA]);
    col3 = _mm256_load_pd(&A[4 + (i + 3) * ldA]);

    temp010 = _mm256_unpacklo_pd(col0, col1);
    temp011 = _mm256_unpackhi_pd(col0, col1);
    temp230 = _mm256_unpacklo_pd(col2, col3);
    temp231 = _mm256_unpackhi_pd(col2, col3);

    col0 = _mm256_permute2f128_pd(temp010, temp230, 0x20);
    col1 = _mm256_permute2f128_pd(temp011, temp231, 0x20);
    col2 = _mm256_permute2f128_pd(temp010, temp230, 0x31);
    col3 = _mm256_permute2f128_pd(temp011, temp231, 0x31);

    _mm256_store_pd(&B[i + 4 * ldB], col0);
    _mm256_store_pd(&B[i + 1 + 4 * ldB], col1);
    _mm256_store_pd(&B[i + 2 + 4 * ldB], col2);
    _mm256_store_pd(&B[i + 3 + 4 * ldB], col3);
  }  
}
#endif
