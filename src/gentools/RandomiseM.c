#include "gentools/RandomiseM.h"
/*
  Given a pointer A to an m x n matrix with leading dimension ldA,
  updates the entries of A with random doubles.
 */

void randomiseM(int m, int n, double* A, int ldA){
  for (int i = 0; i < m; ++i){
    for (int j = 0; j < n; ++j){
      *(A + i + ldA * j) = (double) rand() / (double)RAND_MAX;
    }
  }
}
