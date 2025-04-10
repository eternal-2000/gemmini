#include "testing.h"
/*
  Reports performance of the test dgemm functoin, without comparison to BLAS.
 */

void solo_test(char* transA, char* transB,
	       int init_n, int final_n, int inc, int reps){
  unsigned long sq;
  double gflops;
  double best;
  double average = 0.0;
  int num_trials = ((final_n - init_n)/inc) * reps;

  for (int n = init_n; n <= final_n; n += inc){
    sq = n * n;
    gflops = 2 * sq * n * 1e-09;

    double* A = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* B = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* C = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);

    if (!A || !B || !C){
      fprintf(stderr, "Failed to allocate memory to matrices.\n");
      exit(EXIT_FAILURE);
    }

    randomiseM(n, n, A, n);
    randomiseM(n, n, B, n);
    randomiseM(n, n, C, n);

    double s = 1.0;    
    best = 0.;
    for (int t = 0; t < reps; ++t){
      double start = omp_get_wtime();
      testgemm(transA, transB, n, n, n, s, A, n, B, n, C, n);
      double end = omp_get_wtime();

      double exec_time = end - start;
      double perf = gflops / exec_time;
      average += perf;
      if (perf > best) best = perf;
    }

    printf("%d %f\n", n, best);
    fflush(stdout);
    _mm_free(A);
    _mm_free(B);
    _mm_free(C);
  }

  average /= num_trials;
  printf("Average: %f\n", average);
}

int main(int argc, char** argv){
  int numargs = 7;
  argcount(numargs - 1, argc - 1);
  
  char* transA = argv[1];
  char* transB = argv[2];
  int init_n = atoi(argv[3]), final_n = atoi(argv[4]), inc = atoi(argv[5]), reps = atoi(argv[6]);

  init_n = (init_n/inc) * inc;
  final_n = (final_n/inc) * inc;

  solo_test(transA, transB, init_n, final_n, inc, reps);  
  return 0;
}
