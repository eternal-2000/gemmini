#ifndef PACKING_H
#define PACKING_H

#include "minmax.h"
#include<string.h>
#include<omp.h>

void packMicroA(char*, int, int, double*, int, double*);
void packMicroB(char*, int, int, double, double*, int, double*);
void packA(char*, int, int, double*, int, double*);
void packB(char*, int, int, double, double*, int, double*);

void packMicroA_32(char*, int, int, float*, int, float*);
void packMicroB_32(char*, int, int, float, float*, int, float*);
void packA_32(char*, int, int, float*, int, float*);
void packB_32(char*, int, int, float, float*, int, float*);

#endif
