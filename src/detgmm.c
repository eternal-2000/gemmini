// Default values for MR and NR in case not given at compile time

#ifndef MR
#define MR 8
#endif

#ifndef NR
#define NR 8
#endif

#include "kernel.h"
#include "minmax.h"
#include <stdio.h>

/*
  Deterministic dense double-precision general matrix multiplication (dgemm)
  ==================================================
  Implements C += AB, where A is m x k, B is k x n, and C is m x n, and all entries assumed to be
  double-precision floating point numbers.
 */

// Default value for block sizes to fit in cache
#ifndef MC
#define MC 144
#endif

#ifndef NC
#define NC 144
#endif

#ifndef KC
#define KC 144
#endif

void (*kernel)(int, double*, int, double*, int, double*, int) = NULL;

void init_kernel(){
  if (MR == 4 && NR == 4){
    kernel = kernel4x4;
  } else if (MR == 8 && NR == 4){
    kernel = kernel8x4;
  } else if (MR == 8 && NR == 8){
    kernel = kernel8x8;
  } else{
    fprintf(stderr, "Kernel type %dx%d unsupported", MR, NR);
    exit(EXIT_FAILURE);
  }
}

void cached_kernel(int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  for (int j = 0; j < n; j += NR){
    for (int i = 0; i < m; i += MR){
      kernel(p, &A[i], ldA, &B[j * ldB], ldB, &C[i + j * ldC], ldC);
    }
  }
}

void detgmm(int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  init_kernel();

  if (m % MR != 0 || MC % MR != 0){
    fprintf(stderr, "m: %d\n MC: %d\n MR: %d\n but m and MC must be multiples of MR\n", m, MC, MR);
    exit(EXIT_FAILURE);
  }

  if (n % NR != 0 || NC % NR != 0){
    fprintf(stderr, "n: %d\n NC: %d\n NR: %d\n but n and NC must be multiples of NR\n", n, NC, NR);
    exit(EXIT_FAILURE);
  }

  for (int k = 0; k < p; k += KC){
    int k_b = MIN(KC, p - k);
    for (int i = 0; i < m; i += MC){
      int i_b = MIN(MC, m - i);
      cached_kernel(i_b, n, k_b, &A[i + k * ldA], ldA, &B[k], ldB, &C[i], ldC);
    } 
  }
}

