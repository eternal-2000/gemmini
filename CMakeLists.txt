cmake_minimum_required(VERSION 3.10)
project(RGeMM C) # C only, for now

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

# Source files shared among both driver and accuracy tests
set(SHARED_SOURCES
  src/testgemm.c
  src/RandomiseM.c
  src/detgmm.c
  src/kernel4x4.c
  src/kernel8x4.c
  src/kernel8x8.c
)

##################################################
# Main driver program
##################################################

add_executable(driver
  src/driver.c
  ${SHARED_SOURCES}
  src/test_performance.c
)

target_link_libraries(driver PRIVATE shared_settings)

# Optional accuracy test: defaults to ON
option(CHECK_ACCURACY "Build accuracy tests" ON)

if(CHECK_ACCURACY)
  add_executable(test_accuracy
    src/test_accuracy.c
    src/mdiff.c
    ${SHARED_SOURCES}
  )

  target_link_libraries(test_accuracy PRIVATE shared_settings)

  add_custom_command(
    TARGET test_accuracy
    POST_BUILD
    COMMAND $<TARGET_FILE:test_accuracy>
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Testing accuracy against BLIS reference implementation..."
  )

endif()

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
