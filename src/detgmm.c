#include "detgmm.h"
void detgmm(char* transA, char* transB,
	    int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  if ((strcmp(transA, "T") && strcmp(transA, "N")) || (strcmp(transB, "T") && (strcmp(transB, "N")))){
    fprintf(stderr, "Invalid transpose arguments: %s, %s\n", transA, transB);
    exit(EXIT_FAILURE);
  }  
  for (int j = 0; j < n; j += NC){
    int jb = MIN(NC, n - j);
    proc_B(transA, transB, m, jb, p, A, ldA, &B[j * ldB], ldB, &C[j * ldC], ldC);
  }
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
  double* A_pack = (double*) _mm_malloc(MC * KC * (sizeof(double)), CACHE_ALIGN);
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
  if (ldC % VEC_WIDTH != 0){ // Align C to AVX register width (4 doubles) if needed, and update
    mk_avx_buffer_C(m, n, p, A_pack, B_pack, C, ldC);
  } else{
    for (int i = 0; i < m; i += MR){
      int ib = MIN(MR, m - i);
      if (ib < MR || n < NR){ // If not enough rows and columns, assign a temp buffer
	mk_part_buffer_C(ib, n, p, &A_pack[i * p], B_pack, &C[i], ldC);
      } else{
	MICROKERNEL(MR, NR)(p, &A_pack[i * p], B_pack, &C[i], ldC);
      }
    }
  }
}
