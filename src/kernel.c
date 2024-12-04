#include "kernel.h"

void kernel4x4(int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  /*
    Load vector registers with a 4 x 4 block of C. Fills up four AVX registers with four floats (__m256d).
    _mm256_loadu_pd needs pointer to address of microblock of C, then loads up 4 consecutive elements of
    C (in column-major order).
  */

  __m256d c_0123_0 = _mm256_loadu_pd(&C[0]);
  __m256d c_0123_1 = _mm256_loadu_pd(&C[ldC]);
  __m256d c_0123_2 = _mm256_loadu_pd(&C[2 * ldC]);
  __m256d c_0123_3 = _mm256_loadu_pd(&C[3 * ldC]);

  for (int k = 0; k < p; ++k){
    /*
      Load vector registers with a column of 4 elements of A, then broadcast (k,j) elements of B.
      Then execute FMA to update elements of C by scaling column of A with elements of the row of B.
     */
    
    __m256d a_0123_k = _mm256_loadu_pd(&A[k * ldA]);
    __m256d b_k_j = _mm256_broadcast_sd(&B[k]);
    c_0123_0 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_0);

    b_k_j = _mm256_broadcast_sd(&B[k + ldB]);
    c_0123_1 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_1);
    
    b_k_j = _mm256_broadcast_sd(&B[k + 2 * ldB]);
    c_0123_2 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_2);

    b_k_j = _mm256_broadcast_sd(&B[k + 3 * ldB]);
    c_0123_3 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_3);
  }
  /*
    Save updated doubles as new elements of C
   */

  _mm256_storeu_pd(&C[0], c_0123_0);
  _mm256_storeu_pd(&C[ldC], c_0123_1);
  _mm256_storeu_pd(&C[2 * ldC], c_0123_2);
  _mm256_storeu_pd(&C[3 * ldC], c_0123_3);
}
