#include "gentools/buffers.h"

void mk_avx_buffer_C(int m, int n, int p, double* A, double* B, double* C, int ldC){
  double* C_buff = (double*) _mm_malloc(MR * NR *sizeof(double), CACHE_ALIGN);
  for (int i = 0; i < m; i += MR){
    memset(C_buff, 0, MR * NR * sizeof(double));
    MICROKERNEL(MR, NR)(p, &A[i * p], B, C_buff, MR);

    int ib = MIN(MR, m - i);
    for (int jc = 0; jc < n; ++jc){
      for (int ic = 0; ic < ib; ++ic){
	C[i + ic + jc * ldC] += C_buff[ic + jc * MR];
      }
    }
  }
  _mm_free(C_buff);
}
