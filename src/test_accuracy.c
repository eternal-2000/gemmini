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
    double* A = (double*) malloc(sq * sizeof(double));
    double* B = (double*) malloc(sq * sizeof(double));
    double* C = (double*) malloc(sq * sizeof(double));
    double* C_ref = (double*) malloc(sq * sizeof(double));
    
    if (!A || !B || !C){
      fprintf(stderr, "Failed to allocate memory to matrices.\n");
      exit(EXIT_FAILURE);
    }

    randomiseM(n, n, A, n);
    randomiseM(n, n, B, n);
    randomiseM(n, n, C, n);
    memcpy(C_ref, C, n * ldC * sizeof(double));
    
    worst = 0.;
    for (int t = 0; t < reps; ++t){
      testgemm(n, n, n, A, n, B, n, C, n);
      dgemm_("No transpose", "No transpose", &n, &n, &n,
	     &1.0, A, &ldA,
	     B, &ldB,
	     &1.0, C_ref, &ldC);
      
      double err = mdiff(n, n, C, n, C_ref, n);
      if (err > worst) worst = err;
    }

    printf("%d %f\n", n, worst);

    fflush(stdout);
    free(A);
    free(B);
    free(C);
    free(C_ref);
  }
}
