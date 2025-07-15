#ifndef SGEMM_H
#define SGEMM_H

#include "kernels/sgemm_kernel.h"
#include "gentools/packing.h"
#include "gentools/minmax.h"
#include "gentools/buffers.h"
#include "memparams.h"
#include "globaltypes.h"
#include<stdio.h>
#include<string.h>
#include<omp.h>

void sgemm(transOpt, transOpt, int, int, int, float, float*, int, float*, int, float*, int);
void proc_B_32(transOpt, transOpt, int, int, int, float, float*, int, float*, int, float*, int);
void proc_A_32(transOpt, int, int, int, float*, int, float*, float*, int);
void macrokernel_32(int, int, int, float*, float*, float*, int);

#endif
