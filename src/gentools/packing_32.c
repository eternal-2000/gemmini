#include "gentools/packing.h"
/** --------------------------------------------------
    Single precision
    --------------------------------------------------
*/

void packMicroA_32(transOpt transA, int m, int p, float* A, int ldA, float* A_pack){
  if (transA == NoTrans){
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

void packA_32(transOpt transA, int m, int p, float* A, int ldA, float* A_pack){
  #pragma omp parallel for
  for (int i = 0; i < m; i += MR){
    int ib = MIN(MR, m - i);
    packMicroA_32(transA, ib, p, &A[i], ldA, &A_pack[i * p]);
  }
}

void packMicroB_32(transOpt transB, int p, int n, float alpha, float* B, int ldB, float* B_pack){
  if (transB == NoTrans){
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

void packB_32(transOpt transB, int p, int n, float alpha, float* B, int ldB, float* B_pack){
  #pragma omp parallel for
  for (int j = 0; j < n; j += NR){
    int jb = MIN(NR, n - j);
    packMicroB_32(transB, p, jb, alpha, &B[j * ldB], ldB, &B_pack[j * p]);
  }
}
