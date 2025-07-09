#include "testgemm.h"

void test_dgemm(char* transA, char* transB,
	      int m, int n, int p, double alpha, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  /** A wrapper for a particular dgemm implementation. Trivial for now but will change in future. */
  
  dgemm(transA, transB, m, n, p, alpha, A, ldA, B, ldB, C, ldC);
}
