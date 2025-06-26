#ifndef TESTGEMM_H
#define TESTGEMM_H

#include "dgemm.h"
#include "sgemm.h"

void testgemm(char*, char*, int, int, int, double, double*, int, double*, int, double*, int);
void test_sgemm(char*, char*, int, int, int, float, float*, int, float*, int, float*, int);

#endif
