#include<stdio.h>
#include<stdlib.h>
#include "test_performance.h"
/*
  TODO: Include way to call particular function to test from command line as a string
 */

int main(int argc, char** argv){
  int numargs = 7;
  if (numargs != argc){
    fprintf(stderr, "Wrong number of arguments: expected %d, received %d\n",
	    numargs - 1, argc - 1);
    exit(EXIT_FAILURE);
  }
  char* transA = argv[1];
  char* transB = argv[2];
  int init_n = atoi(argv[3]), final_n = atoi(argv[4]), inc = atoi(argv[5]), reps = atoi(argv[6]);

  init_n = (init_n/inc) * inc;
  final_n = (final_n/inc) * inc;

  test_perf(transA, transB, init_n, final_n, inc, reps);
  
  return 0;  
}
