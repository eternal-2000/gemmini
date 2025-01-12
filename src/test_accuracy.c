/*
  Compares accuracy of test dgemm implementation to reference BLIS implementation
  Tests with inputs as n x n square matrices starting from n = init_n up to n = final_n in increments of inc.
  Records biggest difference between implementations, measured as the biggest difference between elements in
  the final result, repeats (reps) times, and saves biggest difference.

  Outputs to stdout rows of the form
                n          diff
  where diff is a double representing biggest difference over all trials for size n.
 */
#include "RandomiseM.h"
#include "testgemm.h"
#include "mdiff.h"
#include <stdio.h>
#include <string.h>
#include <immintrin.h>

// Prototype of BLIS dgemm
void dgemm_(char*, char*, int*, int*, int*,
	    double*, double*, int*,
	    double*, int*,
	    double*, double*, int*);

void test_accuracy(int init_n, int final_n, int inc, int reps){
  unsigned long sq;
  double worst;
  for (int n = init_n; n <= final_n; n += inc){
    sq = n * n;
    double* A = (double*) _mm_malloc(sq * sizeof(double), 64);
    double* B = (double*) _mm_malloc(sq * sizeof(double), 64);
    double* C = (double*) _mm_malloc(sq * sizeof(double), 64);
    double* C_ref = (double*) _mm_malloc(sq * sizeof(double), 64);
    
    if (!A || !B || !C){
      fprintf(stderr, "Failed to allocate memory to matrices.\n");
      exit(EXIT_FAILURE);
    }

    randomiseM(n, n, A, n);
    randomiseM(n, n, B, n);
    randomiseM(n, n, C, n);
    memcpy(C_ref, C, sq * sizeof(double));
    
    worst = 0.;
    double one = 1.0;
    for (int t = 0; t < reps; ++t){
      testgemm(n, n, n, A, n, B, n, C, n);
      dgemm_("No transpose", "No transpose", &n, &n, &n,
	     &one, A, &n,
	     B, &n,
	     &one, C_ref, &n);
      
      double err = mdiff(n, n, C, n, C_ref, n);
      if (err > worst) worst = err;
    }

    printf("%d %e\n", n, worst);

    fflush(stdout);
    _mm_free(A);
    _mm_free(B);
    _mm_free(C);
    _mm_free(C_ref);
  }
}

int main(void){
  test_accuracy(48, 960, 48, 3);

  return 0;
}
