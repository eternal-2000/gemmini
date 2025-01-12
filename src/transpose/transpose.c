#include<immintrin.h>

/*
  TODO: Currently performs well for small matrices, and performs very well for large powers of 2.

  Add kernels for matrix sizes which are not powers of 2. Include Gflop/sec count as performance metric.
 */

static void transpose4x4(const double* restrict A, int ldA, double* restrict B, int ldB){
  /*
    Transposes a 4x4 block of doubles A and stores it in B
   */
  __m256d col0 = _mm256_loadu_pd(&A[0]);
  __m256d col1 = _mm256_loadu_pd(&A[ldA]);
  __m256d col2 = _mm256_loadu_pd(&A[2 * ldA]);
  __m256d col3 = _mm256_loadu_pd(&A[3 * ldA]);

  /* Manual implementation of _MM_TRANSPOSE4_PD if it existed:
     The following intrinsics will shuffle elements in columns of A around until col1, ..., col4 have elements
     matching the rows of A instead. 
   */
  
  __m256d temp010 = _mm256_unpacklo_pd(col0, col1);
  __m256d temp011 = _mm256_unpackhi_pd(col0, col1);
  __m256d temp230 = _mm256_unpacklo_pd(col2, col3);
  __m256d temp231 = _mm256_unpackhi_pd(col2, col3);

  col0 = _mm256_permute2f128_pd(temp010, temp230, 0x20);
  col1 = _mm256_permute2f128_pd(temp011, temp231, 0x20);
  col2 = _mm256_permute2f128_pd(temp010, temp230, 0x31);
  col3 = _mm256_permute2f128_pd(temp011, temp231, 0x31);

  /* End "_MM_TRANSPOSE4_PD" */
  
  _mm256_storeu_pd(&B[0], col0);
  _mm256_storeu_pd(&B[ldB], col1);
  _mm256_storeu_pd(&B[2 * ldB], col2);
  _mm256_storeu_pd(&B[3 * ldB], col3); 
}

static void transpose16x16(const double* restrict A, int ldA, double* restrict B, int ldB){
  for (int i = 0; i < 16; i += 4){
    for (int j = 0; j < 16; j += 4){
      __m256d col0 = _mm256_loadu_pd(&A[j + i * ldA]);
      __m256d col1 = _mm256_loadu_pd(&A[j + (i + 1) * ldA]);
      __m256d col2 = _mm256_loadu_pd(&A[j + (i + 2) * ldA]);
      __m256d col3 = _mm256_loadu_pd(&A[j + (i + 3) * ldA]);

      __m256d temp010 = _mm256_unpacklo_pd(col0, col1);
      __m256d temp011 = _mm256_unpackhi_pd(col0, col1);
      __m256d temp230 = _mm256_unpacklo_pd(col2, col3);
      __m256d temp231 = _mm256_unpackhi_pd(col2, col3);

      col0 = _mm256_permute2f128_pd(temp010, temp230, 0x20);
      col1 = _mm256_permute2f128_pd(temp011, temp231, 0x20);
      col2 = _mm256_permute2f128_pd(temp010, temp230, 0x31);
      col3 = _mm256_permute2f128_pd(temp011, temp231, 0x31);

      _mm256_storeu_pd(&B[i + j * ldB], col0);
      _mm256_storeu_pd(&B[i + (j + 1) * ldB], col1);
      _mm256_storeu_pd(&B[i + (j + 2) * ldB], col2);
      _mm256_storeu_pd(&B[i + (j + 3) * ldB], col3);
    }
  }
}

/* static void transpose12x12(const double* restrict A, int ldA, double* restrict B, int ldB){
  
} */

int nextbit(int n){ // Halves n if exact power of 2, otherwise returns most significant bit
  if (!(n & (n - 1))) return n >> 1;
  return 1 << (31 - __builtin_clz(n));
}

void transpose(int m, int n, const double* restrict A, int ldA, double* restrict B, int ldB){
  if (m <= 16 && n <= 16){ // Base cases
    if (m == 16 && n == 16){
      transpose16x16(A, ldA, B, ldB);
    } else{
      for (int i = 0; i < m; ++i){
	for (int j = 0; j < n; ++j){
	  B[j + i * ldB] = A[i + j * ldA];
	}
      } 
    }
    return;
  }

  /*
    Otherwise, partition the matrix into a 2 x 2 block and recurse on each of the blocks
    Partitions are chosen by taking the largest submatrices which have dimensions equal to exact powers of 2,
    then recursing.    
   */

  int split_m = nextbit(m);
  int split_n = nextbit(n);

  transpose(split_m, split_n, A, ldA, B, ldB); 
  transpose(m - split_m, split_n, &A[split_m], ldA, &B[split_m * ldB], ldB); 
  transpose(split_m, n - split_n, &A[split_n * ldA], ldA, &B[split_n], ldB); 
  transpose(m - split_m, n - split_n, &A[split_m + split_n * ldA], ldA, &B[split_n + split_m * ldB], ldB);
}

#ifdef UNIT_TEST
#include<stdio.h>
#include<stdlib.h>
#include<time.h>
#include<string.h>

void naive_transpose(int m, int n, const double* restrict A, int ldA, double* B, int ldB){
  for (int i = 0; i < m; ++i){
    for (int j = 0; j < n; ++j){
      B[j + i * ldB] = A[i + j * ldA];
    }
  }
}

void randomiseM(int m, int n, double* A, int ldA){
  for (int i = 0; i < m; ++i){
    for (int j = 0; j < n; ++j){
      *(A + i + j * ldA) = (double) rand() / (double) RAND_MAX;
    }
  }
}

void printMatrix(int m, int n, const double* restrict A, int ldA){
  for (int i = 0; i < m; ++i){
    for (int j = 0; j < n; ++j){
      printf("%.6f ", A[i + j * ldA]);
    }
    printf("\n");
  }
}

int main(int argc, char** argv){
  int numargs = 5;
  if (argc != numargs){
    fprintf(stderr, "Wrong number of arguments: expected %d, received %d\n",
	    numargs - 1, argc - 1);
    exit(EXIT_FAILURE);
  }

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
    double* A = (double*) malloc(sq * sizeof(double));
    double* A_trans = (double*) malloc(sq * sizeof(double));
    double* A_ref = (double*) malloc(sq * sizeof(double));

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
      printMatrix(n, n, A, n);
      printf("Naive transpose:\n");
      printMatrix(n, n, A_ref, n);
      printf("Recursive SIMD kernel transpose:\n");
      printMatrix(n, n, A_trans, n);

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

#endif
