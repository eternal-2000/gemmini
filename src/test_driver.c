#include<stdio.h>
#include<stlib.h>
#include "test_performance.h"

int main(int argc, char** argv){
  const int numargs = 5;

  if (argc != numargs){
    fprintf(stderr, "Incorrect number of arguments supplied: requires %d, received %d\n",
	    numargs - 1, argc - 1);
    exit(EXIT_FAILURE);
  }

  int init_n = atoi(argv[1]), final_n = atoi(argv[2]), inc = atoi(argv[3]), reps = atoi(argv[4]);
  init_n = (init_n/inc) * inc;
  final_n = (final_n/inc) * inc;

  test_perf(init_n, final_n, inc, reps);
  return 0;  
}
