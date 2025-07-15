#include "sgemm.h"

void sgemm(transOpt transA, transOpt transB,
	   int m, int n, int p,
	   float alpha, float* A, int ldA,
	   float* B, int ldB,
	   float* C, int ldC){

  if (alpha == 0) return;
  
  int thread_count = omp_get_max_threads();
  int mt_C_block_size = ((NC / thread_count) / NR) * NR;
  int full_cols = (n/(mt_C_block_size * thread_count)) * thread_count * mt_C_block_size;
  int rem = n - full_cols;
  int rem_per_thread = (1 + (rem/thread_count) / NR) * NR;

#pragma omp parallel for
  for (int j = 0; j < full_cols; j += mt_C_block_size){
    proc_B_32(transA, transB,
	      m, mt_C_block_size, p,
	      alpha, A, ldA,
	      &B[j * ldB], ldB,
	      &C[j * ldC], ldC);
  }

#pragma omp parallel for
  for (int j = full_cols; j < n; j += rem_per_thread){
    int jb = MIN(rem_per_thread, n - j);
    proc_B_32(transA, transB,
	      m, jb, p,
	      alpha, A, ldA,
	      &B[j * ldB], ldB,
	      &C[j * ldC], ldC);
  }
}

void proc_B_32(transOpt transA, transOpt transB,
	       int m, int n, int p,
	       float alpha, float* A, int ldA,
	       float* B, int ldB,
	       float* C, int ldC){
  float* B_pack = (float*) _mm_malloc(PC * NC * sizeof(float), CACHE_ALIGN);
  for (int k = 0; k < p; k += PC){
    int kb = MIN(PC, p - k);
    packB_32(transB, kb, n, alpha, &B[k], ldB, B_pack);
    proc_A_32(transA,
	      m, n, kb,
	      &A[k * ldA], ldA,
	      B_pack,
	      C, ldC);
  }
  _mm_free(B_pack);
}

void proc_A_32(transOpt transA,
	       int m, int n, int p,
	       float* A, int ldA,
	       float* B_pack,
	       float* C, int ldC){
  float* A_pack = (float*) _mm_malloc(MC * PC * sizeof(float), CACHE_ALIGN);
  for (int i = 0; i < m; i += MC){
    int ib = MIN(MC, m - i);
    packA_32(transA, ib, p, &A[i], ldA, A_pack);
    macrokernel_32(ib, n, p,
		   A_pack, B_pack,
		   &C[i], ldC);
  }
  _mm_free(A_pack);
}

void macrokernel_32(int m, int n, int p,
		    float* A_pack, float* B_pack,
		    float* C, int ldC){
  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    if (ldC % VEC_WIDTH_SGL != 0){ // Align C to vector register width if needed, and update
      mk_avx_buffer_32(m, jb, p, A_pack, &B_pack[j * p], &C[j * ldC], ldC);
    } else{
      for (int i = 0; i < m; i += MR){
	int ib = MIN(MR, m - i);
	if (ib < MR || jb < NR){
	  mk_part_buffer_32(ib, jb, p, &A_pack[i * p], &B_pack[j * p], &C[i + j * ldC], ldC);
	} else{
	  sgemm_kernel(p, &A_pack[i * p], &B_pack[j * p], &C[i + j * ldC], ldC);
	}
      }
    }
  }
}
