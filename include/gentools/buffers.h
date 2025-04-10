#ifndef BUFFERS_H
#define BUFFERS_H

#include<immintrin.h>
#include<string.h>
#include "kernels/kernel.h"
#include "gentools/minmax.h"
#include "memparams.h"

void mk_part_buffer(int, int, int, double*, double*, double*, int);
void mk_avx_buffer(int, int, int, double*, double*, double*, int);

#endif
