#include "dgemm.h"
void dgemm(char* transA, char* transB,
	    int m, int n, int p, double alpha, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  if ((strcmp(transA, "T") && strcmp(transA, "N")) || (strcmp(transB, "T") && (strcmp(transB, "N")))){
    fprintf(stderr, "Invalid transpose arguments: %s, %s\n", transA, transB);
    exit(EXIT_FAILURE);
  }

  if (alpha == 0) return;
  
  int thread_count = omp_get_max_threads();
  int mt_C_block_size = ((NC / thread_count) / NR) * NR;
  int full_cols = (n/(mt_C_block_size * thread_count)) * thread_count * mt_C_block_size;
  int rem = n - full_cols;
  int rem_per_thread = (1 + (rem/thread_count) / NR) * NR;

#pragma omp parallel for
  for (int j = 0; j < full_cols; j += mt_C_block_size){
    proc_B_64(transA, transB, m, mt_C_block_size, p, alpha, A, ldA, &B[j * ldB], ldB, &C[j * ldC], ldC);
  }

#pragma omp parallel for
  for (int j = full_cols; j < n; j += rem_per_thread){
    int jb = MIN(rem_per_thread, n - j);
    proc_B_64(transA, transB, m, jb, p, alpha, A, ldA, &B[j * ldB], ldB, &C[j * ldC], ldC);
  }
}

void proc_B_64(char* transA, char* transB, int m, int n, int p, double alpha, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  double* B_pack = (double*) _mm_malloc(PC * NC * (sizeof(double)), CACHE_ALIGN);
  for (int k = 0; k < p; k += PC){
    int kb = MIN(PC, p - k);
    packB_64(transB, kb, n, alpha, &B[k], ldB, B_pack);
    proc_A_64(transA, m, n, kb, &A[k * ldA], ldA, B_pack, C, ldC);
  }
  _mm_free(B_pack);
}

void proc_A_64(char* transA, int m, int n, int p, double* A, int ldA, double* B_pack, double* C, int ldC){
  double* A_pack = (double*) _mm_malloc(MC * PC * sizeof(double), CACHE_ALIGN);
  for (int i = 0; i < m; i += MC){
    int ib = MIN(MC, m - i);
    packA_64(transA, ib, p, &A[i], ldA, A_pack);
    macrokernel_64(ib, n, p, A_pack, B_pack, &C[i], ldC);
  }
  _mm_free(A_pack);
}

void macrokernel_64(int m, int n, int p, double* A_pack, double* B_pack, double* C, int ldC){ // Target ASM from here
  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    if (ldC % VEC_WIDTH != 0){ // Align C to vector register width if needed, and update
      mk_avx_buffer_64(m, jb, p, A_pack, &B_pack[j * p], &C[j * ldC], ldC);
    } else{
      for (int i = 0; i < m; i += MR){
	int ib = MIN(MR, m - i);
	if (ib < MR || jb < NR){ // If block is not full, calculate with a temp buffer
	  mk_part_buffer_64(ib, jb, p, &A_pack[i * p], &B_pack[j * p], &C[i + j * ldC], ldC);
	} else{
	  dgemm_kernel(p, &A_pack[i * p], &B_pack[j * p], &C[i + j * ldC], ldC);
	}
      }
    }
  }
}
