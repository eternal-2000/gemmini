#include "gentools/packing.h"
/**
  Implements packing micropanels of matrices A and B, and stores them
  in (resp.) A_pack, B_pack. If a matrix cannot be tiled with MR x NR
  micropanels, pads the micropanel with zeros to full MR x NR size.

  Note: there is an asymmetry between the packing of A and B because
  during the packing of B we also scale it by the scalar α in the
  operation C += αAB.
 */

/** --------------------------------------------------
    Double precision
    --------------------------------------------------
 */

void packMicroA_64(char* transA, int m, int p, double* A, int ldA, double* A_pack){
  if (!strcmp(transA, "N")){
    for (int k = 0; k < p; ++k){
      for (int i = 0; i < m; ++i){
	*A_pack++ = A[i + k * ldA];
      }
      for (int i = m; i < MR; ++i){
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
  }
}

void packA_64(char* transA, int m, int p, double* A, int ldA, double* A_pack){
  #pragma omp parallel for
  for (int i = 0; i < m; i += MR){
    int ib = MIN(MR, m - i);
    packMicroA_64(transA, ib, p, &A[i], ldA, &A_pack[i * p]);
  }
}

void packMicroB_64(char* transB, int p, int n, double alpha, double* B, int ldB, double* B_pack){
  if (!strcmp(transB, "N")){
    for (int k = 0; k < p; ++k){
      for (int j = 0; j < n; ++j){
	*B_pack++ = alpha * B[k + j * ldB];
      }
      for (int j = n; j < NR; ++j){ 
	*B_pack++ = 0.0;
      }
    }
  } else{
    for (int k = 0; k < p; ++k){
      for (int j = 0; j < n; ++j){
	*B_pack++ = alpha * B[j + k * ldB];
      }
      for (int j = n; j < NR; ++j){
	*B_pack++ = 0.0;
      }
    }
  }
}

void packB_64(char* transB, int p, int n, double alpha, double* B, int ldB, double* B_pack){
  #pragma omp parallel for
  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    packMicroB_64(transB, p, jb, alpha, &B[j * ldB], ldB, &B_pack[j * p]);
  }
}
