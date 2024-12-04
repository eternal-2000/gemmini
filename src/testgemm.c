#include "detgmm.h"
#include "testgemm.h"

void testgemm(int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  detgmm(m, n, p, A, ldA, B, ldB, C, ldC);
}
