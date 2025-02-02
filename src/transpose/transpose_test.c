#include<stdlib.h>
#include<time.h>
#include<string.h>
#include "memparams.h"
#include "transpose/transpose.h"
#include "gentools/print_matrix.h"
#include "gentools/RandomiseM.h"
#include "gentools/argcount.h"

void naive_transpose(int m, int n, double* A, int ldA, double* B, int ldB){
  for (int i = 0; i < m; ++i){
    for (int j = 0; j < n; ++j){
      B[j + i * ldB] = A[i + j * ldA];
    }
  }
}

int main(int argc, char** argv){
  int numargs = 5;
  argcount(numargs - 1, argc - 1);

  int init_n = atoi(argv[1]), final_n = atoi(argv[2]), inc = atoi(argv[3]), reps = atoi(argv[4]);

  init_n = (init_n/inc) * inc;
  final_n = (final_n/inc) * inc;

  srand(time(NULL));

  clock_t start, end;
  double time, perf;

  printf("Dimension | Naive      | Blocked    (GB/sec)\n");
  
  for (int n = init_n; n < final_n; n += inc){
    unsigned long sq = n * n;
    double GB = 16 * sq * 1e-9; // Total amount of bytes transferred (1 read, 1 write, 8 bytes per double)
    double* A = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* A_trans = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);
    double* A_ref = (double*) _mm_malloc(sq * sizeof(double), CACHE_ALIGN);

    double best_naive = 0.0, best_blocked = 0.0;
    
    if (!A || !A_trans || !A_ref){
      fprintf(stderr, "Failed to allocate memory to matrices\n");
      exit(EXIT_FAILURE);
    }

    randomiseM(n, n, A, n);
    
    printf("%-10d ", n);

    for (int t = 0; t < reps; ++t){
      start = clock();
      naive_transpose(n, n, A, n, A_ref, n);
      end = clock();
      time = (double) (end - start)/CLOCKS_PER_SEC;
      perf = GB/time;
      best_naive = (perf > best_naive) ? perf : best_naive; 
    }

    printf("%10.3f ", best_naive);

    for (int t = 0; t < reps; ++t){
      start = clock();
      transpose(n, n, A, n, A_trans, n);
      end = clock();
      time = (double) (end - start)/CLOCKS_PER_SEC;
      perf = GB/time;
      best_blocked = (perf > best_blocked) ? perf : best_blocked;
    }
    
    printf("%10.3f\n", best_blocked);
    
    if (memcmp(A_trans, A_ref, sq * sizeof(double))){
      fprintf(stderr, "Transpose failed at matrix dimensions %d x %d\n", n, n);
      print_matrix(n, n, A, n);
      printf("Naive transpose:\n");
      print_matrix(n, n, A_ref, n);
      printf("Recursive SIMD kernel transpose:\n");
      print_matrix(n, n, A_trans, n);

      free(A);
      free(A_trans);
      free(A_ref);
      exit(EXIT_FAILURE);
      }

    free(A);
    free(A_trans);
    free(A_ref);
    fflush(stdout);
  }
  printf("All tests passed\n");
  return 0;
}
