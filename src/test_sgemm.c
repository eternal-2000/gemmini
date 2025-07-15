#include "testgemm.h"

void test_sgemm(transOpt transA, transOpt transB,
		int m, int n, int p,
		float alpha, float* A, int ldA,
		float* B, int ldB,
		float* C, int ldC){
  /** A wrapper for a particular sgemm implementation.
      Trivial for now but will change in future. */
  
  sgemm(transA, transB, m, n, p, alpha, A, ldA, B, ldB, C, ldC);
}
