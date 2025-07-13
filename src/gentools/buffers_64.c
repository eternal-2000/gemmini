#include "gentools/buffers.h"
#include "kernels/dgemm_kernel.h"

void mk_avx_buffer_64(int m, int n, int p, double* A, double* B, double* C, int ldC){
  double* C_buff = (double*) _mm_malloc(MR * NR *sizeof(double), CACHE_ALIGN);
  for (int i = 0; i < m; i += MR){
    memset(C_buff, 0, MR * NR * sizeof(double));
    dgemm_kernel(p, &A[i * p], B, C_buff, MR);

    int ib = MIN(MR, m - i);
    for (int jc = 0; jc < n; ++jc){
      for (int ic = 0; ic < ib; ++ic){
	C[i + ic + jc * ldC] += C_buff[ic + jc * MR];
      }
    }
  }
  _mm_free(C_buff);
}


void mk_part_buffer_64(int m, int n, int p, double* A, double* B, double* C, int ldC){
  /**
     Handles incomplete microtile of C by passing a full MR x NR
     buffer of zeros to the microkernel, then accumulating the results
     into the incomplete microtile.
   */

  double* C_buff = (double*) _mm_malloc(MR * NR * sizeof(double), CACHE_ALIGN);
  memset(C_buff, 0, MR * NR * sizeof(double));
  dgemm_kernel(p, A, B, C_buff, MR);

  for (int j = 0; j < n; ++j){
    for (int i = 0; i < m; ++i){
      C[i + j * ldC] += C_buff[i + j * MR];
    }
  }
  _mm_free(C_buff);
}
