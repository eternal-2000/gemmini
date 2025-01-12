#include "kernel4x4.h"

void kernel4x4(int p, double* A, double* B, double* C, int ldC){
  /*
    Loads vector registers with 4 x 4 block of C. Fills up 4 AVX registers with 4 floats (__m256d) each.
    Then a column of A takes an AVX register and elements of a row of B are successively broadcast into 1 more.
  */
  __m256d c_0123_0 = _mm256_loadu_pd(&C[0]);
  __m256d c_0123_1 = _mm256_loadu_pd(&C[ldC]);
  __m256d c_0123_2 = _mm256_loadu_pd(&C[2 * ldC]);
  __m256d c_0123_3 = _mm256_loadu_pd(&C[3 * ldC]);
  __m256d b_kj;
  
  for (int k = 0; k < p; ++k){
    __m256d a_0123_k = _mm256_loadu_pd(A);
    b_kj = _mm256_broadcast_sd(&B[0]);
    c_0123_0 = _mm256_fmadd_pd(a_0123_k, b_kj, c_0123_0);

    b_kj = _mm256_broadcast_sd(&B[1]);
    c_0123_1 = _mm256_fmadd_pd(a_0123_k, b_kj, c_0123_1);

    b_kj = _mm256_broadcast_sd(&B[2]);
    c_0123_2 = _mm256_fmadd_pd(a_0123_k, b_kj, c_0123_2);

    b_kj = _mm256_broadcast_sd(&B[3]);
    c_0123_3 = _mm256_fmadd_pd(a_0123_k, b_kj, c_0123_3);

    A += MR;
    B += NR;
  }

  // TODO: Consider when a microtile may be only partly complete

  _mm256_storeu_pd(&C[0], c_0123_0);
  _mm256_storeu_pd(&C[ldC], c_0123_1);
  _mm256_storeu_pd(&C[2 * ldC], c_0123_2);
  _mm256_storeu_pd(&C[3 * ldC], c_0123_3);
}
