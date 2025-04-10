#ifndef TEST_ACCURACY_H
#define TEST_ACCURACY_H

#include "testgemm.h"
#include "gentools/argcount.h"
#include "gentools/mdiff.h"
#include "gentools/RandomiseM.h"
#include <stdio.h>
#include <string.h>
#include <immintrin.h>

// Prototype of BLAS dgemm
void dgemm_(char*, char*, int*, int*, int*,
	    double*, double*, int*,
	    double*, int*,
	    double*, double*, int*);
void test_accuracy(char*, char*, int, int, int, int);

#endif
