#ifndef PACKING_H
#define PACKING_H

#include "minmax.h"
#include "globaltypes.h"
#include<string.h>
#include<omp.h>

void packMicroA_64(transOpt, int, int, double*, int, double*);
void packMicroB_64(transOpt, int, int, double, double*, int, double*);
void packA_64(transOpt, int, int, double*, int, double*);
void packB_64(transOpt, int, int, double, double*, int, double*);

void packMicroA_32(transOpt, int, int, float*, int, float*);
void packMicroB_32(transOpt, int, int, float, float*, int, float*);
void packA_32(transOpt, int, int, float*, int, float*);
void packB_32(transOpt, int, int, float, float*, int, float*);

#endif
