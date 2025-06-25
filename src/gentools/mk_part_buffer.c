#include "gentools/buffers.h"

void mk_part_buffer(int m, int n, int p, double* A, double* B, double* C, int ldC){
  /**
    Handles incomplete microtile of C by passing a full MR x NR buffer of zeros to the
    microkernel, then accumulating the results into the incomplete microtile.
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
