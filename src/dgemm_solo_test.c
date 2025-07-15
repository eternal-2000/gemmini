#include "testing.h"
/**
  Reports performance of the test dgemm function, without comparison to BLAS.
 */

void dgemm_solo_test(transOpt transA, transOpt transB,
		     int init_n, int final_n, int inc, int reps){
  unsigned long sq;
  double gflops;
  double best;
  double average = 0.0;
  int num_trials = ((final_n - init_n)/inc) * reps;

  /** Warmup section to allow more accurate first readings **/

  double* warmup_A = (double*) _mm_malloc(64 * 64 * sizeof(double), CACHE_ALIGN);
  double* warmup_B = (double*) _mm_malloc(64 * 64 * sizeof(double), CACHE_ALIGN);
  double* warmup_C = (double*) _mm_malloc(64 * 64 * sizeof(double), CACHE_ALIGN);
  
  randomiseM(64, 64, warmup_A, 64);
  randomiseM(64, 64, warmup_B, 64);
  randomiseM(64, 64, warmup_C, 64);
  
  for (int i = 0; i < 3; i++) {
    test_dgemm(transA, transB, 64, 64, 64, 1.0, warmup_A, 64, warmup_B, 64, warmup_C, 64);
  }
  
  _mm_free(warmup_A);
  _mm_free(warmup_B);
  _mm_free(warmup_C);  

  /** End of warmup section **/
  
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
      test_dgemm(transA, transB,
		 n, n, n,
		 s, A, n,
		 B, n,
		 C, n);
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
  
  transOpt transA = parseTrans(argv[1]);
  transOpt transB = parseTrans(argv[2]);
  int init_n = atoi(argv[3]), final_n = atoi(argv[4]), inc = atoi(argv[5]), reps = atoi(argv[6]);

  init_n = (init_n/inc) * inc;
  final_n = (final_n/inc) * inc;

  dgemm_solo_test(transA, transB, init_n, final_n, inc, reps);  
  return 0;
}
