#include "kernel4x12.h"

void kernel4x12(int p, double* A, double* B, double* C, int ldC){
  /*
    Loads vector registers with 4 x 12 block of C. Fills up 12 AVX registers with 4 floats each.
    Then a column of A takes an AVX register and elements of a row of B are successively broadcast into 1 more.
  */

  __m256d c_0123[12];
  for (int j = 0; j < 12; ++j){
    c_0123[j] = _mm256_loadu_pd(&C[j * ldC]);
  }

  for (int k = 0; k < p; ++k){
    __m256d a_0123_k = _mm256_loadu_pd(&A[k * MR]);
    for (int j = 0; j < 12; ++j){
      __m256d b_k_j = _mm256_broadcast_sd(&B[j + k * NR]);
      c_0123[j] = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123[j]);
    }
  }

  for (int j = 0; j < 12; ++j){
    _mm256_storeu_pd(&C[j * ldC], c_0123[j]);
  }  
}
