#include "gentools/argcount.h"

void argcount(int a, int b){
  if (a != b){
    fprintf(stderr, "Wrong number of arguments: expected %d, received %d\n", a, b);
    exit(EXIT_FAILURE);
  }
}
