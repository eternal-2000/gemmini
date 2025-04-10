#ifndef TEST_PERFORMANCE_H
#define TEST_PERFORMANCE_H

#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<immintrin.h>
#include<time.h>
#include "testgemm.h"
#include "gentools/RandomiseM.h"

/* Prototype of BLIS reference dgemm */
void dgemm_(char*, char*,
	    int* , int*, int*,
	    double*, double*, int*,
	    double*, int*,
	    double*, double*, int*);
void test_perf(char*, char*, int, int, int, int);

#endif
