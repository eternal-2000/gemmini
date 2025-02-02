#include "kernels/kernel4x12.h"

void kernel4x12(const int p, double* restrict A, double* restrict B, double* restrict C, const int ldC){
  // TODO: Unroll me!
  
  __m256d c_0123[12];
  for (int j = 0; j < 12; ++j){
    c_0123[j] = _mm256_load_pd(&C[j * ldC]);
  }

  for (int k = 0; k < p; ++k){
    __m256d a_0123_k = _mm256_load_pd(&A[k * MR]);
    for (int j = 0; j < 12; ++j){
      __m256d b_k_j = _mm256_broadcast_sd(&B[j + k * NR]);
      c_0123[j] = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123[j]);
    }
  }

  for (int j = 0; j < 12; ++j){
    _mm256_store_pd(&C[j * ldC], c_0123[j]);
  }  
}
