#include "kernels/kernel16x4.h"

void kernel16x4(const int p, double* restrict A, double* restrict B, double* restrict C, const int ldC){
  __m256d c_0123_0 = _mm256_load_pd(&C[0]);
  __m256d c_0123_1 = _mm256_load_pd(&C[ldC]);
  __m256d c_0123_2 = _mm256_load_pd(&C[2 * ldC]);
  __m256d c_0123_3 = _mm256_load_pd(&C[3 * ldC]);

  __m256d c_4567_0 = _mm256_load_pd(&C[4]);
  __m256d c_4567_1 = _mm256_load_pd(&C[4 + ldC]);
  __m256d c_4567_2 = _mm256_load_pd(&C[4 + 2 * ldC]);
  __m256d c_4567_3 = _mm256_load_pd(&C[4 + 3 * ldC]);

  __m256d c_89AB_0 = _mm256_load_pd(&C[8]);
  __m256d c_89AB_1 = _mm256_load_pd(&C[8 + ldC]);
  __m256d c_89AB_2 = _mm256_load_pd(&C[8 + 2 * ldC]);
  __m256d c_89AB_3 = _mm256_load_pd(&C[8 + 3 * ldC]);

  __m256d c_CDEF_0 = _mm256_load_pd(&C[12]);
  __m256d c_CDEF_1 = _mm256_load_pd(&C[12 + ldC]);
  __m256d c_CDEF_2 = _mm256_load_pd(&C[12 + 2 * ldC]);
  __m256d c_CDEF_3 = _mm256_load_pd(&C[12 + 3 * ldC]);

  __m256d a_0123_k, a_4567_k, a_89AB_k, a_CDEF_k, b_k_j;
  
  for (int k = 0; k < p; ++k){
    a_0123_k = _mm256_load_pd(&A[0]);
    a_4567_k = _mm256_load_pd(&A[4]);
    a_89AB_k = _mm256_load_pd(&A[8]);
    a_CDEF_k = _mm256_load_pd(&A[12]);
    
    b_k_j = _mm256_broadcast_sd(&B[0]);
    c_0123_0 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_0);
    c_4567_0 = _mm256_fmadd_pd(a_4567_k, b_k_j, c_4567_0);
    c_89AB_0 = _mm256_fmadd_pd(a_89AB_k, b_k_j, c_89AB_0);
    c_CDEF_0 = _mm256_fmadd_pd(a_CDEF_k, b_k_j, c_CDEF_0);
        
    b_k_j = _mm256_broadcast_sd(&B[1]);
    c_0123_1 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_1);
    c_4567_1 = _mm256_fmadd_pd(a_4567_k, b_k_j, c_4567_1);
    c_89AB_1 = _mm256_fmadd_pd(a_89AB_k, b_k_j, c_89AB_1);
    c_CDEF_1 = _mm256_fmadd_pd(a_CDEF_k, b_k_j, c_CDEF_1);
    
    b_k_j = _mm256_broadcast_sd(&B[2]);
    c_0123_2 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_2);
    c_4567_2 = _mm256_fmadd_pd(a_4567_k, b_k_j, c_4567_2);
    c_89AB_2 = _mm256_fmadd_pd(a_89AB_k, b_k_j, c_89AB_2);
    c_CDEF_2 = _mm256_fmadd_pd(a_CDEF_k, b_k_j, c_CDEF_2);
    
    b_k_j = _mm256_broadcast_sd(&B[3]);
    c_0123_3 = _mm256_fmadd_pd(a_0123_k, b_k_j, c_0123_3);
    c_4567_3 = _mm256_fmadd_pd(a_4567_k, b_k_j, c_4567_3);
    c_89AB_3 = _mm256_fmadd_pd(a_89AB_k, b_k_j, c_89AB_3);
    c_CDEF_3 = _mm256_fmadd_pd(a_CDEF_k, b_k_j, c_CDEF_3);
    
    A += MR;
    B += NR;
  }

  _mm256_store_pd(&C[0], c_0123_0);
  _mm256_store_pd(&C[ldC], c_0123_1);
  _mm256_store_pd(&C[2 * ldC], c_0123_2);
  _mm256_store_pd(&C[3 * ldC], c_0123_3);

  _mm256_store_pd(&C[4], c_4567_0);
  _mm256_store_pd(&C[4 + ldC], c_4567_1);
  _mm256_store_pd(&C[4 + 2 * ldC], c_4567_2);
  _mm256_store_pd(&C[4 + 3 * ldC], c_4567_3);

  _mm256_store_pd(&C[8], c_89AB_0);
  _mm256_store_pd(&C[8 + ldC], c_89AB_1);
  _mm256_store_pd(&C[8 + 2 * ldC], c_89AB_2);
  _mm256_store_pd(&C[8 + 3 * ldC], c_89AB_3);

  _mm256_store_pd(&C[12], c_CDEF_0);
  _mm256_store_pd(&C[12 + ldC], c_CDEF_1);
  _mm256_store_pd(&C[12 + 2 * ldC], c_CDEF_2);
  _mm256_store_pd(&C[12 + 3 * ldC], c_CDEF_3);
}
