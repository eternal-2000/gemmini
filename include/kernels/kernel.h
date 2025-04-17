#ifndef KERNEL_H
#define KERNEL_H
#include "kernel4x4.h"
#include "kernel8x4.h"
#include "kernel8x6.h"
#include "kernel8x8.h"
#include "kernel4x12.h"
#include "kernel12x8.h"
#include "kernel12x4.h"
#include "kernel12x3.h"
#include "kernel12x2.h"
#include "kernel12x1.h"
#include "kernel16x4.h"

#define KERNEL_SIZE(M,N) kernel##M##x##N
#define MICROKERNEL(M, N) KERNEL_SIZE(M,N)

#endif
