#include "gentools/print_matrix.h"

void print_matrix(const int m, const int n, const double* restrict A, const int ldA){
  for (int i = 0; i < m; ++i){
    for (int j = 0; j < n; ++j){
      printf("%f ", A[i + j * ldA]);
    }
    printf("\n");    
  }
}
