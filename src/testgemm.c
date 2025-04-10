#include "detgmm.h"
#include "testgemm.h"

void testgemm(char* transA, char* transB,
	      int m, int n, int p, double alpha, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  detgmm(transA, transB, m, n, p, alpha, A, ldA, B, ldB, C, ldC);
}
