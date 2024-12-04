#include "kernel8x8.h"

void kernel8x8(int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  /*
    Loads vector registers with 8 x 8 block of C. Fills up 16 AVX registers with 4 floats each.
    Then a column of A takes 2 more AVX registers, and then a row of B is broadcast into 1 more.
   */
  __m256d c_0123_0 = _mm256_loadu_pd(&C[0]);
  __m256d c_0123_1 = _mm256_loadu_pd(&C[ldC]);
  __m256d c_0123_2 = _mm256_loadu_pd(&C[2 * ldC]);
  __m256d c_0123_3 = _mm256_loadu_pd(&C[3 * ldC]);
  __m256d c_0123_4 = _mm256_loadu_pd(&C[4 * ldC]);
  __m256d c_0123_5 = _mm256_loadu_pd(&C[5 * ldC]);
  __m256d c_0123_6 = _mm256_loadu_pd(&C[6 * ldC]);
  __m256d c_0123_7 = _mm256_loadu_pd(&C[7 * ldC]);
  
  __m256d c_4567_0 = _mm256_loadu_pd(&C[4]);
  __m256d c_4567_1 = _mm256_loadu_pd(&C[4 + ldC]);
  __m256d c_4567_2 = _mm256_loadu_pd(&C[4 + 2 * ldC]);
  __m256d c_4567_3 = _mm256_loadu_pd(&C[4 + 3 * ldC]);
  __m256d c_4567_4 = _mm256_loadu_pd(&C[4 + 4 * ldC]);
  __m256d c_4567_5 = _mm256_loadu_pd(&C[4 + 5 * ldC]);
  __m256d c_4567_6 = _mm256_loadu_pd(&C[4 + 6 * ldC]);
  __m256d c_4567_7 = _mm256_loadu_pd(&C[4 + 7 * ldC]);
  
  for (int k = 0; k < p; ++k){
      __m256d a_0123_k = _mm256_loadu_pd(&A[k * ldA]);
      __m256d a_4567_k = _mm256_loadu_pd(&A[4 + k * ldA]);
      __m256d b_k_j = _mm256_broadcast_sd(&B[k]);

      c_0123_0 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_0);
      c_4567_0 = _mm256_fmadd_pd(a_4567_k, b_k_j, c_4567_0);

      b_k_j = _mm256_broadcast_sd(&B[k + ldB]);
      c_0123_1 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_1);
      c_4567_1 = _mm256_fmadd_pd(a_4567_k, b_k_j, c_4567_1);

      b_k_j = _mm256_broadcast_sd(&B[k + 2 * ldB]);
      c_0123_2 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_2);
      c_4567_2 = _mm256_fmadd_pd(a_4567_k, b_k_j, c_4567_2);
      
      b_k_j = _mm256_broadcast_sd(&B[k + 3 * ldB]);
      c_0123_3 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_3);
      c_4567_3 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_4567_3);

      b_k_j = _mm256_broadcast_sd(&B[k + 4 * ldB]);
      c_0123_4 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_4);
      c_4567_4 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_4567_4);

      b_k_j = _mm256_broadcast_sd(&B[k + 5 * ldB]);
      c_0123_5 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_5);
      c_4567_5 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_4567_5);

      b_k_j = _mm256_broadcast_sd(&B[k + 6 * ldB]);
      c_0123_6 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_6);
      c_4567_6 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_4567_6);

      b_k_j = _mm256_broadcast_sd(&B[k + 7 * ldB]);
      c_0123_7 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_7);
      c_4567_7 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_4567_7);
    }
  /*
    Save updated vectors as new elements of C
   */
  _mm256_storeu_pd(&C[0], c_0123_0);
  _mm256_storeu_pd(&C[4], c_4567_0);
  _mm256_storeu_pd(&C[ldC], c_0123_1);
  _mm256_storeu_pd(&C[4 + ldC], c_4567_1);
  _mm256_storeu_pd(&C[2 * ldC], c_0123_2);
  _mm256_storeu_pd(&C[4 + 2 * ldC], c_4567_2);
  _mm256_storeu_pd(&C[3 * ldC], c_0123_3);
  _mm256_storeu_pd(&C[4 + 3 * ldC], c_4567_3);
  _mm256_storeu_pd(&C[4 * ldC], c_0123_4);
  _mm256_storeu_pd(&C[4 + 4 * ldC], c_4567_4);
  _mm256_storeu_pd(&C[5 * ldC], c_0123_5);
  _mm256_storeu_pd(&C[4 + 5 * ldC], c_4567_5);
  _mm256_storeu_pd(&C[6 * ldC], c_0123_6);
  _mm256_storeu_pd(&C[4 + 6 * ldC], c_4567_6);
  _mm256_storeu_pd(&C[7 * ldC], c_0123_7);
  _mm256_storeu_pd(&C[4 + 7 * ldC], c_4567_7);
}
