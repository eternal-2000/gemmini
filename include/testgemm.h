#ifndef TESTGEMM_H
#define TESTGEMM_H

#include "dgemm.h"
#include "sgemm.h"

void test_dgemm(transOpt, transOpt, int, int, int, double, double*, int, double*, int, double*, int);
void test_sgemm(transOpt, transOpt, int, int, int, float, float*, int, float*, int, float*, int);

#endif
