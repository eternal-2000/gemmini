#include "gentools/packing.h"
/*
  Implements packing micropanels of matrices A and B, and stores them in (resp.) A_pack, B_pack
  If a matrix cannot be tiled with MR x NR micropanels then pads the micropanel with zeros.
 */

void packMicroA(char* transA, int m, int p, double* A, int ldA, double* A_pack){
  if (!strcmp(transA, "N")){
    for (int k = 0; k < p; ++k){
      for (int i = 0; i < m; ++i){
	*A_pack++ = A[i + k * ldA];
      }
      for (int i = m; i < MR; ++i){
	*A_pack++ = 0.0;
      }
    }
    for (int k = p; k < KC; ++k){
      for (int i = 0; i < MR; ++i){
	*A_pack++ = 0.0;
      }
    }
  } else{
    for (int k = 0; k < p; ++k){
      for (int i = 0; i < m; ++i){
	*A_pack++ = A[k + i * ldA];
      }
      for (int i = m; i < MR; ++i){
	*A_pack++ = 0.0;
      }
    }
    for (int k = p; k < KC; ++k){
      for (int i = 0; i < MR; ++i){
	*A_pack++ = 0.0;
      }
    }
  }
}

void packA(char* transA, int m, int p, double* A, int ldA, double* A_pack){
  for (int i = 0; i < m; i += MR){
    int ib = MIN(MR, m - i);
    packMicroA(transA, ib, p, &A[i], ldA, A_pack);
    A_pack += ib * p;
  }
}

void packMicroB(char* transB, int p, int n, double* B, int ldB, double* B_pack){
  if (!strcmp(transB, "N")){
    for (int k = 0; k < p; ++k){
      for (int j = 0; j < n; ++j){
	*B_pack++ = B[k + j * ldB];
      }
      for (int j = n; j < NR; ++j){
	*B_pack++ = 0.0;
      }
    }
    for (int k = p; k < KC; ++k){
      for (int i = 0; i < MR; ++i){
	*B_pack++ = 0.0;
      }
    }
  } else{
    for (int k = 0; k < p; ++k){
      for (int j = 0; j < n; ++j){
	*B_pack++ = B[j + k * ldB];
      }
      for (int j = n; j < NR; ++j){
	*B_pack++ = 0.0;
      }
    }
    for (int k = p; k < KC; ++k){
      for (int i = 0; i < MR; ++i){
	*B_pack++ = 0.0;
      }
    }
  }
}

void packB(char* transB, int p, int n, double* B, int ldB, double* B_pack){
  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    packMicroB(transB, p, jb, &B[j * ldB], ldB, B_pack);
    B_pack += p * jb;
  }
}
