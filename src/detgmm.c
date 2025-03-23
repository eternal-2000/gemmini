#include "detgmm.h"
void detgmm(char* transA, char* transB,
	    int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  if ((strcmp(transA, "T") && strcmp(transA, "N")) || (strcmp(transB, "T") && (strcmp(transB, "N")))){
    fprintf(stderr, "Invalid transpose arguments: %s, %s\n", transA, transB);
    exit(EXIT_FAILURE);
  }

  int thread_count = omp_get_max_threads();
  int mt_C_block_size = ((NC / thread_count) / NR) * NR;
  int full_cols = (n/(mt_C_block_size * thread_count)) * thread_count * mt_C_block_size;
  int rem = n - full_cols;
  int rem_per_thread = (1 + (rem/thread_count) / NR) * NR;

  #pragma omp parallel for
  for (int j = 0; j < full_cols; j += mt_C_block_size){ // Load-balanced columns
    proc_B(transA, transB, m, mt_C_block_size, p, A, ldA, &B[j * ldB], ldB, &C[j * ldC], ldC);
  }

  #pragma omp parallel for
  for (int j = full_cols; j < n; j += rem_per_thread){
    int jb = MIN(rem_per_thread, n - j);
    proc_B(transA, transB, m, jb, p, A, ldA, &B[j * ldB], ldB, &C[j * ldC], ldC);
  }

  /* for (int j = 0; j < n; j += NC){ */
  /*   int jb = MIN(NC, n - j); */
  /*   proc_B(transA, transB, m, jb, p, A, ldA, &B[j * ldB], ldB, &C[j * ldC], ldC); */
  /* } */
}

void proc_B(char* transA, char* transB,
	    int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  double* B_pack = (double*) _mm_malloc(KC * NC * (sizeof(double)), CACHE_ALIGN);
  for (int k = 0; k < p; k += KC){
    int kb = MIN(KC, p - k);
    packB(transB, kb, n, &B[k], ldB, B_pack);
    proc_A(transA, m, n, kb, &A[k * ldA], ldA, B_pack, C, ldC);
  }
  _mm_free(B_pack);
}

void proc_A(char* transA,
	    int m, int n, int p, double* A, int ldA, double* B_pack, double* C, int ldC){
  double* A_pack = (double*) _mm_malloc(MC * KC * sizeof(double), CACHE_ALIGN);

  /* int max_thread_count = omp_get_max_threads(); */
  /* double* A_pack = (double*) _mm_malloc(MC * KC * max_thread_count * sizeof(double), CACHE_ALIGN); */

  /* int full_blocks = (m/(max_thread_count * MC)) * MC * max_thread_count; */
  /* int rem = m - full_blocks; */
  /* int rem_per_thread = (1 + (rem/max_thread_count) / MR) * MR; */

  /* #pragma omp parallel for */
  /* for (int i = 0; i < full_blocks; i += MC){ // Load-balanced rows */
  /*   int ib = MC; */
  /*   packA(transA, ib, p, &A[i], ldA, &A_pack[MC * KC * omp_get_thread_num()]); */
  /*   macrokernel(ib, n, p, &A_pack[MC * KC * omp_get_thread_num()], B_pack, &C[i], ldC); */
  /* } */

  /* #pragma omp parallel for */
  /* for (int i = full_blocks; i < m; i += rem_per_thread){ // Unbalanced rows */
  /*   int ib = MIN(rem_per_thread, m - i); */
  /*   packA(transA, ib, p, &A[i], ldA, &A_pack[MC * KC * omp_get_thread_num()]); */
  /*   macrokernel(ib, n, p, &A_pack[MC * KC * omp_get_thread_num()], B_pack, &C[i], ldC); */
  /* } */
  
  for (int i = 0; i < m; i += MC){
    int ib = MIN(MC, m - i);
    packA(transA, ib, p, &A[i], ldA, A_pack);
    macrokernel(ib, n, p, A_pack, B_pack, &C[i], ldC);
  }
  _mm_free(A_pack);
}

void macrokernel(int m, int n, int p, double* A_pack, double* B_pack, double* C, int ldC){
  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    mkger(m, jb, p, A_pack, &B_pack[j * p], &C[j * ldC], ldC);
  }  
}

void mkger(int m, int n, int p, double* A_pack, double* B_pack, double* C, int ldC){
  if (ldC % VEC_WIDTH != 0){ // Align C to vector register width if needed, and update
    mk_avx_buffer_C(m, n, p, A_pack, B_pack, C, ldC);
  } else{
    for (int i = 0; i < m; i += MR){
      int ib = MIN(MR, m - i);
      if (ib < MR || n < NR){ // If block is not full, calculate with a temp buffer
	mk_part_buffer_C(ib, n, p, &A_pack[i * p], B_pack, &C[i], ldC);
      } else{
	MICROKERNEL(MR, NR)(p, &A_pack[i * p], B_pack, &C[i], ldC);
      }
    }
  }
}
