cmake_minimum_required(VERSION 3.10)
project(RGeMM C) # C only, for now

# Check in /include for headers
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

add_executable(testGeMM
  src/testGeMM.c
  src/RandomiseM.c)

# Set compiler flags
target_compile_options(testGeMM PRIVATE
  -Wall
  -O3
  -march=native
  -funroll-loops
  -ffast-math
  -fassociative-math
  -mfma
  -mavx # Note: Does not always improve performance on Ryzen 5 5600X
  -flto
)

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
