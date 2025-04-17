#ifndef TRANSPOSE_H
#define TRANSPOSE_H

#include "transpose4x4.h"
#include "transpose8x8.h"
#include "transpose16x16.h"
#include "nextbit.h"

void transpose(int, int, double* restrict, int, double* restrict, int);

#endif
