#ifndef DGEMM_H
#define DGEMM_H

#include "kernels/dgemm_kernel.h"
#include "gentools/packing.h"
#include "gentools/minmax.h"
#include "gentools/buffers.h"
#include "memparams.h"
#include "globaltypes.h"
#include<stdio.h>
#include<string.h>
#include<omp.h>

void dgemm(transOpt, transOpt, int, int, int, double, double*, int, double*, int, double*, int);
void proc_B_64(transOpt, transOpt, int, int, int, double, double*, int, double*, int, double*, int);
void proc_A_64(transOpt, int, int, int, double*, int, double*, double*, int);
void macrokernel_64(int, int, int, double*, double*, double*, int);

#endif
