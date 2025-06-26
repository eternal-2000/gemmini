#include "gentools/buffers.h"
#include "kernels/sgemm_kernel.h"

void mk_part_buffer_32(int m, int n, int p, float* A, float* B, float* C, int ldC){
  float* C_buff = (float*) _mm_malloc(MR * NR * sizeof(float), CACHE_ALIGN);
  memset(C_buff, 0, MR * NR * sizeof(float));
  sgemm_kernel(p, A, B, C_buff, MR);

  for (int j = 0; j < n; ++j){
    for (int i = 0; i < m; ++i){
      C[i + j * ldC] += C_buff[i + j * MR];
    }
  }
  _mm_free(C_buff);
}

void mk_avx_buffer_32(int m, int n, int p, float* A, float* B, float* C, int ldC){
  float* C_buff = (float*) _mm_malloc(MR * NR * sizeof(float), CACHE_ALIGN);
  for (int i = 0; i < m; i += MR){
    memset(C_buff, 0, MR * NR * sizeof(float));
    sgemm_kernel(p, &A[i * p], B, C_buff, MR);

    int ib = MIN(MR, m - i);
    for (int jc = 0; jc < n; ++jc){
      for (int ic = 0; ic < ib; ++ic){
	C[i + ic + jc * ldC] += C_buff[ic + jc * MR];
      }
    }
  }
  _mm_free(C_buff);
}
