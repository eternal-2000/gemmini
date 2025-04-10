#ifndef DETGMM_H
#define DETGMM_H

#include "kernels/kernel.h"
#include "gentools/packing.h"
#include "gentools/minmax.h"
#include "gentools/buffers.h"
#include "transpose/transpose.h"
#include "memparams.h"
#include<stdio.h>
#include<string.h>
#include<omp.h>

void detgmm(char*, char*, int, int, int, double, double*, int, double*, int, double*, int);
void proc_B(char*, char*, int, int, int, double, double*, int, double*, int, double*, int);
void proc_A(char*, int, int, int, double*, int, double*, double*, int);
void macrokernel(int, int, int, double*, double*, double*, int);
void mkger(int, int, int, double*, double*, double*, int);

#endif
