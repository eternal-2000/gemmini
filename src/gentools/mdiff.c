#include "gentools/mdiff.h"

double mdiff(int m, int n, double* A, int ldA, double* B, int ldB){
  /**
    Returns the biggest difference between corresponding entries of matrices A and B    
   */

  double diff = 0.;
  for (int i = 0; i < m; ++i){
    for (int j = 0; j < n; ++j){
      double test = A[i + j * ldA] - B[i + j * ldB];
      test = (test < 0 ? -test : test);
      if (test > diff) diff = test;
    }
  }
  return diff;
}

float mdiff_32(int m, int n, float* A, int ldA, float* B, int ldB){
  float diff = 0.;
  for (int i = 0; i < m; ++i){
    for (int j = 0; j < n; ++j){
      float test = A[i + j * ldA] - B[i + j * ldB];
      test = (test < 0 ? -test : test);
      if (test > diff) diff = test;
    }
  }
  return diff;
}
