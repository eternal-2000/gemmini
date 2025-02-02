#include "gentools/nextbit.h"

int nextbit(int n){ // Halves n if exact power of 2, otherwise returns most significant bit
  if (!(n & (n - 1))) return n >> 1;
  return 1 << (31 - __builtin_clz(n));
}
