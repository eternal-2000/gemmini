shared_sources = files(
  'testgemm.c', 'dgemm.c',
  'gentools/packing.c', 'gentools/mk_part_buffer.c', 'gentools/mk_avx_buffer.c',
  'gentools/RandomiseM.c', 'gentools/nextbit.c', 'transpose/transpose.c',
  'kernels/dgemm_kernel.c'
)

if blis_comp

  acc_sources = files(
    'test_accuracy.c', 'gentools/mdiff.c', 'gentools/argcount.c'
  ) + shared_sources
  
  executable('blis_perf',
             ['driver.c', 'test_performance.c'] + shared_sources,
             include_directories: inc_dir,
             dependencies: blis_dep
            )
  executable('blis_acc',
             ['test_accuracy.c'] + shared_sources + acc_sources,
             include_directories: inc_dir,
             dependencies: blis_dep
            )

  acc = executable('test_accuracy',
                   acc_sources,
                   include_directories: inc_dir,
                   dependencies: blis_dep
                  )

  if get_option('run_accuracy_test')
    test('do_accuracy_tests', acc, args: ['N', 'N', '48', '960', '48', '3'])
  endif
  
endif

executable('solo_test',
  ['solo_test.c', 'gentools/argcount.c'] + shared_sources,
  include_directories: inc_dir,
  dependencies: [dependency('openmp')]
)

subdir('transpose')
