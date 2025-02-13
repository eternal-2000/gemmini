/*
  Compares accuracy of test dgemm implementation to BLAS implementation
  Tests with inputs as n x n square matrices starting from n = init_n up to n = final_n in increments of inc.
  Records biggest difference between implementations, measured as the biggest difference between elements in
  the final result, repeats (reps) times, and saves biggest difference.

  Outputs to stdout rows of the form
                n          diff
  where diff is a double representing biggest difference over all trials for size n.
 */

#include "test_accuracy.h"

void test_accuracy(char* transA, char* transB,
		   int init_n, int final_n, int inc, int reps){
  unsigned long sq;
  double worst;
  for (int n = init_n; n <= final_n; n += inc){
    sq = n * n;
    double* A = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* B = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* C = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* C_ref = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    
    if (!A || !B || !C || !C_ref){
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
      testgemm(transA, transB,
	       n, n, n, A, n, B, n, C, n);
      dgemm_(transA, transB,
	     &n, &n, &n,
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

int main(int argc, char** argv){
  int numargs = 7;
  argcount(numargs - 1, argc - 1);
  
  char* transA = argv[1];
  char* transB = argv[2];
  int init_n = atoi(argv[3]), final_n = atoi(argv[4]), inc = atoi(argv[5]), reps = atoi(argv[6]);

  init_n = (init_n/inc) * inc;
  final_n = (final_n/inc) * inc;
  
  test_accuracy(transA, transB, init_n, final_n, inc, reps);
  return 0;
}
