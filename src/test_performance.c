/*
  Compares performance of test dgemm implemention with reference BLIS implementation.
  Tests with inputs as n x n square matrices starting from n = init_n up to n = final_n in increments of inc.
  Records performance in Gflops/sec, repeats (reps) times, and saves peak performance.

  Outputs to stdout rows of the form
                n          best          best_ref
  where best is a double representing peak performance over all trials for size n, and best_ref is the same
  for the BLIS implementation.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_performance.h"
#include "testgemm.h"

/* Prototype of BLIS reference dgemm */
void dgemm_(char*, char*,
	    int* , int*, int*,
	    double*, double*, int*,
	    double*, int*,
	    double*, double*, int*);

void test_perf(int init_n, int final_n, int inc, int reps){
  unsigned long sq; // Save square of n for convenience
  double gflops; // Gigaflop count for n x n matrix multiplication
  double best, best_ref; // Record best performance for each size

  /* Testing loop */
  
  for (int n = init_n; n <= final_n; n += inc){
    sq = n * n;
    gflops = 2 * sq * n * 1e-09; // Gigaflop count for n x n matrix multiplication 
  
    /* Initialise matrices with random entries */
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
    memcpy(C_ref, C, sq * sizeof(double));
    
    best = 0.;
    for (int t = 0; t < reps; ++t){
      clock_t start = clock();
      testgemm(n, n, n, A, n, B, n, C, n);
      clock_t end = clock();

      double exec_time = (double)(end - start)/CLOCKS_PER_SEC;
      double perf = gflops/exec_time;
      if (perf > best) best = perf;
    }

    best_ref = 0.;
    double one = 1.0;
    for (int t = 0; t < reps; ++t){
      clock_t start_ref = clock();
      dgemm_("No transpose", "No transpose", &n, &n, &n,
	     &one, A, &n,
	     B, &n,
	     &one, C_ref, &n);
      clock_t end_ref = clock();

      double ref_time = (double) (end_ref - start_ref)/CLOCKS_PER_SEC;
      double perf_ref = gflops/ref_time;
      if (perf_ref > best_ref) best_ref = perf_ref;
    }

    /* Send dimensions and best performance to stdout in that order */
    printf("%d %f %f\n", n, best, best_ref);

    fflush(stdout);
    free(A);
    free(B);
    free(C);
    free(C_ref);
  }
}

