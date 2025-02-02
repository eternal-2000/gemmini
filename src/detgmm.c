#include "detgmm.h"

void detgmm(char* transA, char* transB,
	    int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  /*
    Temporary, unoptimised strategy for taking transposes: to be moved inside the packing functions
    and optimised via intrinsics
  */
  
  double* transpose_A = NULL;
  double* transpose_B = NULL;
  
  if (!strcmp(transA, "T")){
    transpose_A = (double*) _mm_malloc(m * p * sizeof(double), CACHE_ALIGN);
    transpose(m, p, A, ldA, transpose_A, m);
    A = transpose_A;
    ldA = p;
    int temp = m; m = p; p = temp;
  } else if (strcmp(transA, "N")){
    fprintf(stderr, "Invalid transA argument: %s\n", transA);
    exit(EXIT_FAILURE);
  }

  if (!strcmp(transB, "T")){
    transpose_B = (double*) _mm_malloc(p * n * sizeof(double), CACHE_ALIGN);
    transpose(p, n, B, ldB, transpose_B, p);
    B = transpose_B;
    ldB = n;
    int temp = p; p = n; n = temp;
  } else if (strcmp(transB, "N")){
    fprintf(stderr, "Invalid transB argument: %s\n", transB);
    exit(EXIT_FAILURE);
  } 

  for (int j = 0; j < n; j += NC){
    int jb = MIN(NC, n - j);
    proc_B(m, jb, p, A, ldA, &B[j * ldB], ldB, &C[j * ldC], ldC);
  }

  if (transpose_A) _mm_free(transpose_A);
  if (transpose_B) _mm_free(transpose_B);
}

void proc_B(int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  double* B_pack = (double*) _mm_malloc(KC * NC * (sizeof(double)), CACHE_ALIGN);
  for (int k = 0; k < p; k += KC){
    int kb = MIN(KC, p - k);
    packB(kb, n, &B[k], ldB, B_pack);
    proc_A(m, n, kb, &A[k * ldA], ldA, B_pack, ldB, C, ldC);
  }
  _mm_free(B_pack);
}

void proc_A(int m, int n, int p, double* A, int ldA, double* B_pack, int ldB, double* C, int ldC){
  double* A_pack = (double*) _mm_malloc(MC * KC * (sizeof(double)), CACHE_ALIGN);
  for (int i = 0; i < m; i += MC){
    int ib = MIN(MC, m - i);
    packA(ib, p, &A[i], ldA, A_pack);
    macrokernel(ib, n, p, A_pack, B_pack, &C[i], ldC);
  }
  _mm_free(A_pack);
}

void macrokernel(int m, int n, int p, double* A_pack, double* B_pack, double* C, int ldC){
  /*
    Marches through packed microtiles of A and loads them up to perform rank-1 updates with packed B
   */
#pragma omp parallel for  
  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    mkger(m, jb, p, A_pack, &B_pack[j * p], &C[j * ldC], ldC);
  }
}

void mkger(int m, int n, int p, double* A_pack, double* B_pack, double* C, int ldC){
  /*
    Multiplies MR x NR blocks in microkernel to perform rank-1 update of micropanel of C.
   */
  for (int i = 0; i < m; i += MR){
    int ib = MIN(MR, m - i);
    MICROKERNEL(MR, NR)(p, &A_pack[i * p], B_pack, &C[i], ldC);
  }
}
