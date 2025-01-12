#include "kernel.h"
#include "packing.h"
#include "minmax.h"
#include <stdio.h>

/*
  Deterministic dense double-precision general matrix multiplication (dgemm)
  ==================================================
  Implements C += AB, where A is m x k, B is k x n, and C is m x n, and all entries assumed to be
  double-precision floating point numbers.
*/

#define KERNEL_SIZE(M,N) kernel##M##x##N
#define MICROKERNEL(M, N) KERNEL_SIZE(M,N)

void make_panels(int, int, int, double*, int, double*, int, double*, int);
void make_mtiles(int, int, int, double*, int, double*, int, double*, int);
void reg_process(int, int, int, double*, int, double*, int, double*, int);
void tilemult(int, int, int, double*, double*, double*, int);
void mtmult(int, int, int, double*, double*, double*, int);

void detgmm(int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  if (m % MR != 0 || MC % MR != 0){
    fprintf(stderr, "m = %d\n MC = %d\n MR = %d\n but m and MC must be multiples of MR\n", m, MC, MR);
    exit(EXIT_FAILURE);
  }

  if (n % NR != 0 || NC % NR != 0){
    fprintf(stderr, "n = %d\n NC = %d\n MR = %d\n but n and NC must be multiples of MR\n", n, NC, NR);
    exit(EXIT_FAILURE);
  }

  make_panels(m, n, p, A, ldA, B, ldB, C, ldC);
}

void make_panels(int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  /*
    Breaks C and B up into panels of NC rows
   */ 
  for (int j = 0; j < n; j += NC){
    int jb = MIN(NC, n - j);
    make_mtiles(m, jb, p, A, ldA, &B[j * ldB], ldB, &C[j * ldC], ldC);
  }
}

void make_mtiles(int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  /*
    Breaks A and B into MC x KC and KC x NC sized blocks, and packs a block of B.
    Packed block of B brought into L3 cache.
   */
  double* B_pack = (double*) _mm_malloc(KC * NC * (sizeof(double)), 64);
  for (int k = 0; k < p; k += KC){
    int kb = MIN(KC, p - k);
    packB(kb, n, &B[k], ldB, B_pack);
    reg_process(m, n, kb, &A[k * ldA], ldA, B_pack, ldB, C, ldC);
  }
  _mm_free(B_pack);
}

void reg_process(int m, int n, int p, double* A, int ldA, double* B_pack, int ldB, double* C, int ldC){
  /*
    Breaks blocks of A and B into MR x KC and KC x NR tiles, and packs tiles of A.
    Then ready to break up and bring microtiles of A and B into registers.
   */
  double* A_pack = (double*) _mm_malloc(MC * KC * sizeof(double), 64);
  for (int i = 0; i < m; i += MC){
    int ib = MIN(MC, m - i);
    packA(ib, p, &A[i], ldA, A_pack);
    tilemult(ib, n, p, A_pack, B_pack, &C[i], ldC);
  }
  _mm_free(A_pack);
}

void tilemult(int m, int n, int p, double* A_pack, double* B_pack, double* C, int ldC){
  /*
    Marches through microtiles of A and loads them up to perform rank-1 updates with packed B
   */
  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    mtmult(m, jb, p, A_pack, &B_pack[j * p], &C[j * ldC], ldC);
  }
}

void mtmult(int m, int n, int p, double* A_pack, double* B_mp, double* C, int ldC){
  /*
    Multiplies a packed microtile of A with a microtile of B. Multiplies MR x NR blocks
    in microkernel to perform rank-1 update of micropanel of C.
   */
  
  for (int i = 0; i < m; i += MR){
    int ib = MIN(MR, m - i);
    MICROKERNEL(MR, NR)(p, &A_pack[i * p], B_mp, &C[i], ldC);
  }
}
