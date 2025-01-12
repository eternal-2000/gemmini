#include "minmax.h"
#include "packing.h"

/*
  Implements packing microtiles
 */

void packMicroA(int m, int p, double* A, int ldA, double* A_pack){
  if (m == MR){
    for (int k = 0; k < p; ++k){
      for (int i = 0; i < MR; ++i){
	*A_pack++ = A[i + k * ldA];
      }
    }
  } else{ // Pad with zeros
    for (int k = 0; k < p; ++k){
      for (int i = 0; i < MR; ++i){
	*A_pack++ = A[i + k * ldA];
      }
      for (int i = MR; i < m; ++i){
	*A_pack++ = 0.0;
      }
    }
  }
}

void packA(int m, int p, double* A, int ldA, double* A_pack){
  for (int i = 0; i < m; i += MR){
    int ib = MIN(MR, m - i);
    packMicroA(ib, p, &A[i], ldA, A_pack);
    A_pack += ib * p;
  }
}

void packMicroB(int p, int n, double* B, int ldB, double* B_pack){
  if (n == NR){
    for (int k = 0; k < p; ++k){
      for (int j = 0; j < NR; ++j){
	*B_pack++ = B[k + j * ldB];
      }
    }
  } else{ // Pad with zeros
    for (int k = 0; k < p; ++k){
      for (int j = 0; j < n; ++j){
	*B_pack++ = B[k + j * ldB];
      }
      for (int j = n; j < NR; ++j){
	*B_pack++ = 0.0;
      }
    }
  }
}

void packB(int p, int n, double* B, int ldB, double* B_pack){
  /*
    Packs a p x n panel of B into a KC x NC buffer in B_pack
   */

  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    packMicroB(p, jb, &B[j * ldB], ldB, B_pack);
    B_pack += p * jb;
  }
}
