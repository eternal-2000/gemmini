#include<stdio.h>
#include<stdlib.h>
#include<time.h>
#include "RandomiseM.h"
#include "kernel.h"
/*
  Test implementation of deterministic dense general matrix multiplication (GeMM), implementing
  conformal partitions and rank-1 updates.
  ==================================================
  Implements C += AB, where A is m x k, B is k x n, and C is m x n, and all entries assumed to be
  double-precision floating point numbers.
 */

// Define block sizes
#define MR 4
#define NR 4

void testgemm(int m, int n, int p, double* A, int ldA, double* B, int ldB, double* C, int ldC){
  /*
    Implements matrix multiplication via blocking
   */
  for (int j = 0; j < n; j += NR){
    for (int i = 0; i < m; i += MR){
      kernel4x4(p, &A[i], ldA, &B[j * ldB], ldB, &C[i + j * ldC], ldC);
    }
  }
}


/*
  TESTING:
  Outputs time and dimension of the matrix to stdout.
  Matrix dimensions are square by convention.
 */

int main(int argc, char** argv){
  const int numargs = 5; // Number of arguments required plus 1 for the program name

  /* Initialisation of variables
     Supply the first four below when calling testgemm
   */
  
  int init_n, // Initial value of n: number of rows and columns
    final_n, // Final value of n
    inc, // Increment to be added to n
    reps, // Number of repetitions to perform for each trial
    sq; // Save square of n for convenience
  double gflops; // Gigaflop count for n x n matrix multiplication
  double best; // Record best performance for each size

  if (argc != numargs){
    fprintf(stderr, "Incorrect number of arguments supplied: requires %d, received %d\n",
	    numargs - 1, argc - 1);
    exit(EXIT_FAILURE);
  }

  init_n = atoi(argv[1]);
  final_n = atoi(argv[2]);
  inc = atoi(argv[3]);
  reps = atoi(argv[4]);
  
  // Adjust init_n and final_n to be multiples of the increment 
  init_n = (init_n/inc) * inc;
  final_n = (final_n/inc) * inc;

  /* Testing loop */
  
  for (int n = init_n; n <= final_n; n += inc){
    sq = n * n;
    gflops = 2 * sq * n * 1e-09; // Gigaflop count for n x n matrix multiplication 
  
    // Initialise matrices with random entries
    double* A = (double*) malloc(sq * sizeof(double)); 
    double* B = (double*) malloc(sq * sizeof(double));
    double* C = (double*) malloc(sq * sizeof(double));

    if (!A || !B || !C){
      fprintf(stderr, "Failed to allocate memory to matrices.\n");
      exit(EXIT_FAILURE);
    }

    randomiseM(n, n, A, n);
    randomiseM(n, n, B, n);
    randomiseM(n, n, C, n);

    // Initialise best performance score for a given size
    best = 0.;
    
    for (int t = 0; t < reps; ++t){
      clock_t start = clock();
      testgemm(n, n, n, A, n, B, n, C, n);
      clock_t end = clock();

      double exec_time = (double)(end - start)/CLOCKS_PER_SEC;
      double perf = gflops/exec_time;
      if (perf > best) best = perf;
    }

    // Send dimensions and best performance to stdout in that order
    printf("%d %f\n", n, best);     
    free(A);
    free(B);
    free(C);    
  }
  return 0;
}
