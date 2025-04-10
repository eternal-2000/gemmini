#include "test_performance.h"

void test_perf(char* transA, char* transB,
	       int init_n, int final_n, int inc, int reps){
  /**
     Compares performance of test dgemm implemention with reference BLIS implementation.
     Tests with inputs as n x n square matrices starting from n = init_n up to n = final_n in increments of inc.
     Records performance in Gflops/sec, repeats (reps) times, and saves peak performance.

     Outputs to stdout rows of the form
     n          best          best_ref
     where best is a double representing peak performance over all trials for size n, and best_ref is the same
     for the BLIS implementation.
  **/
  unsigned long sq; // Save square of n for convenience
  double gflops; // Gigaflop count for n x n matrix multiplication
  double best, best_ref; // Record best performance for each size

  /* Testing loop */
  
  for (int n = init_n; n <= final_n; n += inc){
    sq = n * n;
    gflops = 2 * sq * n * 1e-09; // Gigaflop count for standard n x n matrix multiplication 
  
    /* Initialise matrices with random entries */
    double* A = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN); 
    double* B = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* C = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* C_ref = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    
    if (!A || !B || !C){
      fprintf(stderr, "Failed to allocate memory to matrices.\n");
      exit(EXIT_FAILURE);
    }

    randomiseM(n, n, A, n);
    randomiseM(n, n, B, n);
    randomiseM(n, n, C, n);
    memcpy(C_ref, C, sq * sizeof(double));
    
    double alpha = 1.0;
    double beta = 1.0;
    best = 0.;
    for (int t = 0; t < reps; ++t){
      double start = omp_get_wtime();
      testgemm(transA, transB,
	       n, n, n, alpha, A, n, B, n, C, n);
      double end = omp_get_wtime();

      double exec_time = end - start;
      double perf = gflops/exec_time;
      if (perf > best) best = perf;
    }

    best_ref = 0.;
    for (int t = 0; t < reps; ++t){
      double start_ref = omp_get_wtime();
      dgemm_(transA, transB, &n, &n, &n,
	     &alpha, A, &n,
	     B, &n,
	     &beta, C_ref, &n);
      double end_ref = omp_get_wtime();

      double ref_time = end_ref - start_ref;
      double perf_ref = gflops/ref_time;
      if (perf_ref > best_ref) best_ref = perf_ref;
    }

    /* Send dimensions and best performance to stdout in that order */
    printf("%d %f %f\n", n, best, best_ref);

    fflush(stdout);
    _mm_free(A);
    _mm_free(B);
    _mm_free(C);
    _mm_free(C_ref);
  }
}

