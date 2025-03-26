cmake_minimum_required(VERSION 3.10)
project(GeMMini C) 

# Check in /include for headers
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Add path to reference BLIS library in ~/blis
set(BLIS_PATH "$ENV{HOME}/blis")
include_directories(${BLIS_PATH}/include/blis)
link_directories(${BLIS_PATH}/lib)

find_package(OpenMP REQUIRED)

##################################################
# Shared settings
##################################################

add_library(shared_settings INTERFACE)

target_compile_options(shared_settings INTERFACE
  -Wall
  -O3
  -march=native
  -funroll-loops
  -ffast-math
  -fassociative-math
  -mfma
  -mavx2
  -fopenmp
  -D_POSIX_C_SOURCE=200809L
)

target_include_directories(shared_settings INTERFACE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
  ${BLIS_PATH}/include/blis
)

target_link_libraries(shared_settings INTERFACE
  ${BLIS_PATH}/lib/libblis.a
  pthread
  m
  OpenMP::OpenMP_C
)

# Source files shared among both performance and accuracy tests
set(SHARED_SOURCES
  src/testgemm.c
  src/detgmm.c
  src/gentools/packing.c
  src/gentools/mk_part_buffer_C.c
  src/gentools/mk_avx_buffer_C.c
  src/gentools/RandomiseM.c
  src/gentools/nextbit.c
  src/transpose/transpose.c
  src/kernels/kernel4x4.c
  src/kernels/kernel8x4.c
  src/kernels/kernel8x8.c
  src/kernels/kernel12x4.c
  src/kernels/kernel12x8.c
  src/kernels/kernel4x12.c
  src/kernels/kernel16x4.c  
)

# Files needed for transposition
set(TRANSPOSE_SOURCES
  src/transpose/transpose.c
  src/gentools/nextbit.c
  src/gentools/print_matrix.c
)

# Files needed for sketching
set(SKETCH_SOURCES
  src/sketching/sketch.c
  src/sketching/col_select.c
  src/gentools/print_matrix.c
)

##################################################
# Compile-time constants
##################################################

# Default values for parameters, chosen for Ryzen 5600X
set(MR 8 CACHE STRING "Register row size")
if(MR STREQUAL "OFF")
  set(MR 8)
endif()

set(NR 4 CACHE STRING "Register column size")
if(NR STREQUAL "OFF")
  set(NR 4)
endif()

#set(KR 4 CACHE STRING "Register depth")
set(MC 128 CACHE STRING "Cache row size")
if(MC STREQUAL "OFF")
  set(MC 128)
endif()

set(NC 1028 CACHE STRING "Cache column size")
if(NC STREQUAL "OFF")
  set(NC 1028)
endif()

set(KC 256 CACHE STRING "Cache depth")
if(KC STREQUAL "OFF")
  set(KC 256)
endif()

# Add compile definitions with defaults or user overrides
target_compile_definitions(shared_settings INTERFACE
  MR=${MR}
  NR=${NR}
  MC=${MC}
  NC=${NC}
  KC=${KC}
)

##################################################
# Main driver program - comparison with BLAS
##################################################

add_executable(driver
  src/driver.c
  ${SHARED_SOURCES}
  src/test_performance.c
)

target_link_libraries(driver PRIVATE shared_settings)

# Optional accuracy test: defaults to ON
option(CHECK_ACCURACY "Build accuracy tests" ON)
option(RUN_ACCURACY_TEST "Run accuracy test automatically after build" ON)

if(CHECK_ACCURACY)
  add_executable(test_accuracy
    src/test_accuracy.c
    src/gentools/mdiff.c
    src/gentools/argcount.c
    ${SHARED_SOURCES}
  )

  target_link_libraries(test_accuracy PRIVATE shared_settings)

  if(RUN_ACCURACY_TEST)
    add_custom_command(
      TARGET test_accuracy
      POST_BUILD
      COMMAND $<TARGET_FILE:test_accuracy> N N 48 960 48 3
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT "Testing accuracy against BLIS reference implementation..."
    )
  endif()
endif()

##################################################
# Solo testing - esp. used for autotuning (TBA)
##################################################

add_executable(solo_test
  src/solo_test.c
  ${SHARED_SOURCES}
  src/gentools/argcount.c
)

target_link_libraries(solo_test PRIVATE shared_settings)

# ##################################################
# # Random sketching
# ##################################################

# add_executable(sketch_test
#   src/sketching/sketch_test.c
#   src/gentools/RandomiseM.c
#   src/gentools/argcount.c
#   ${SKETCH_SOURCES}
# )

##################################################
# Transposition
##################################################

add_executable(transpose_test
  src/transpose/transpose_test.c
  ${TRANSPOSE_SOURCES}
  src/gentools/print_matrix.c
  src/gentools/RandomiseM.c
  src/gentools/argcount.c
)

target_link_libraries(transpose_test PRIVATE shared_settings)

##################################################
# Benchmarking and visualisation with Python
##################################################

find_package(Python3 COMPONENTS Interpreter REQUIRED)

set(PLOT_PERF ${CMAKE_CURRENT_SOURCE_DIR}/tests/plotperformance.py)

add_custom_target(plot
  COMMAND ${Python3_EXECUTABLE} ${PLOT_PERF}
  DEPENDS testGeMM # update as more added
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  COMMENT "Generating performance plots..."
)
