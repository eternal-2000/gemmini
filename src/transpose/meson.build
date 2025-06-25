transpose_test_sources = files(
  'transpose_test.c',
  '../gentools/print_matrix.c',
  '../gentools/RandomiseM.c',
  '../gentools/argcount.c',
  '../gentools/nextbit.c',
  'kernels/transpose16x16.c',
  'transpose.c',
)

transpose_test = executable('transpose_test',
  transpose_test_sources,
  include_directories: inc_dir,
  install: false
)

test('transpose_test', transpose_test)
