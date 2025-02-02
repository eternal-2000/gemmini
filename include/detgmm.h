#ifndef DETGMM_H
#define DETGMM_H

#include "kernels/kernel.h"
#include "gentools/packing.h"
#include "gentools/minmax.h"
#include "transpose/transpose.h"
#include "memparams.h"
#include <stdio.h>
#include<string.h>
#include <omp.h>

void proc_A(int, int, int, double*, int, double*, int, double*, int);
void proc_B(int, int, int, double*, int, double*, int, double*, int);
void macrokernel(int, int, int, double*, double*, double*, int);
void mkger(int, int, int, double*, double*, double*, int);
void detgmm(char*, char*, int, int, int, double*, int, double*, int, double*, int);

#endif
