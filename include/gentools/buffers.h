#ifndef BUFFERS_H
#define BUFFERS_H

#include<string.h>
#include "gentools/minmax.h"
#include "memparams.h"

void mk_part_buffer_64(int, int, int, double*, double*, double*, int);
void mk_avx_buffer_64(int, int, int, double*, double*, double*, int);

void mk_part_buffer_32(int, int, int, float*, float*, float*, int);
void mk_avx_buffer_32(int, int, int, float*, float*, float*, int);

#endif
