#include "kernels/kernel12x1.h"

void kernel12x1(const int p, double* restrict A, double* restrict B, double* restrict C, const int ldC){
  __m256d c_0123_0 = _mm256_load_pd(&C[0]);
  __m256d c_4567_0 = _mm256_load_pd(&C[4]);
  __m256d c_89AB_0 = _mm256_load_pd(&C[8]);  
  __m256d b_kj;

  for (int k = 0; k < p; ++k){
    __m256d a_0123_k = _mm256_load_pd(&A[0]);
    __m256d a_4567_k = _mm256_load_pd(&A[4]);
    __m256d a_89AB_k = _mm256_load_pd(&A[8]);

    b_k_j = _mm256_broadcast_sd(&B[0]);
    c_0123_0 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_0);
    c_4567_0 = _mm256_fmadd_pd(a_4567_k, b_k_j, c_4567_0);
    c_89AB_0 = _mm256_fmadd_pd(a_89AB_k, b_k_j, c_89AB_0);

    A += MR;
    B += NR;
  }

  _mm256_store_pd(&C[0], c_0123_0);
  _mm256_store_pd(&C[4], c_4567_0);
  _mm256_store_pd(&C[8], c_89AB_0);
}
