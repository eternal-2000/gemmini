##################################################
#### Sources that do need cache parameters
##################################################

#### Cache parameters set for Ryzen 5 5600X: use generators/get_build_params.py to set them to suit your own CPU!

dgemm_const = [
  '-DMR=8', '-DNR=4', '-DPC=256', '-DMC=128', '-DNC=4096'
]

sgemm_const = [
  '-DMR=8', '-DNR=8', '-DPC=384', '-DMC=256', '-DNC=8192' # Total guess at the last 2 for now
]

dgemm_const_sources = files(
  'dgemm.c',
  'kernels/dgemm_kernel.c',
  'gentools/packing_64.c',
  'gentools/buffers_64.c'
)

dgemm_lib = static_library('dgemm_internal',
                           dgemm_const_sources,
                           c_args: dgemm_const,
                           include_directories: inc_dir
                          )

sgemm_const_sources = files(
  'sgemm.c',
  'kernels/sgemm_kernel.c',
  'gentools/packing_32.c',
  'gentools/buffers_32.c'
)

sgemm_lib = static_library('sgemm_internal',
                           sgemm_const_sources,
                           c_args: sgemm_const,
                           include_directories: inc_dir
                          )

##################################################
#### Sources that do not require cache parameters
##################################################

shared_sources = files(
  'gentools/RandomiseM.c',
  'gentools/nextbit.c',
  'gentools/parseTrans.c',
  'transpose/transpose.c'
)

##################################################
#### Executables 
##################################################

if blis_comp
  acc_sources = files(
    'gentools/mdiff.c', 'gentools/argcount.c'
  ) + shared_sources

  executable('blis_perf',
             ['test_performance.c', 'test_dgemm.c'] + shared_sources,
             link_with: dgemm_lib,
             include_directories: inc_dir,
             dependencies: blis_dep
            )

  executable('blis_acc',
             ['test_accuracy.c', 'test_dgemm.c'] + acc_sources,
             link_with: dgemm_lib,
             include_directories: inc_dir,
             dependencies: blis_dep
            )

  executable('sgemm_acc',
             ['sgemm_test_acc.c', 'test_sgemm.c'] + acc_sources,
             link_with: sgemm_lib,
             include_directories: inc_dir,
             dependencies: blis_dep
            )

  # dgemm_acc = executable('test_accuracy',
  #                        acc_sources,
  #                        link_with: dgemm_lib,
  #                        include_directories: inc_dir,
  #                        dependencies: blis_dep
  #                       )

  # if get_option('run_accuracy_test')
  #   test('do_accuracy_tests', dgemm_acc, args: ['N', 'N', '48', '960', '48', '3'])
  # endif
endif

executable('dgemm_solo_test',
           ['dgemm_solo_test.c', 'test_dgemm.c', 'gentools/argcount.c'] + shared_sources,
           link_with: dgemm_lib,
           include_directories: inc_dir,
           dependencies: [dependency('openmp')]
          )

executable('sgemm_solo_test',
           ['sgemm_solo_test.c', 'test_sgemm.c', 'gentools/argcount.c'] + shared_sources,
           link_with: sgemm_lib,
           include_directories: inc_dir,
           dependencies: [dependency('openmp')]
          )

subdir('transpose')
