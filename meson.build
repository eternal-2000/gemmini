project('Gemmini', 'c', default_options: ['c_std=c11'])

add_project_arguments(
  '-Wall', '-O3', '-march=native', '-funroll-loops',
  '-ffast-math', '-fassociative-math', '-mfma', '-mavx2',
  '-fopenmp', '-D_POSIX_C_SOURCE=200809L',
  '-DMR=' + get_option('MR'),
  '-DNR=' + get_option('NR'),
  '-DMC=' + get_option('MC'),
  '-DNC=' + get_option('NC'),
  '-DPC=' + get_option('PC'),
  language: 'c'
)

cc = meson.get_compiler('c')

inc_dir = [include_directories('include')]
blis_comp = get_option('blis_comp')

if blis_comp

  blis_path = get_option('blis_path')
  if blis_path == ''
    home = run_command('sh', '-c', 'echo $HOME', check: true).stdout().strip()
    blis_path = join_paths(home, 'blis')

    inc_dir += include_directories(join_paths(blis_path, 'include/blis'))
    lib_dir = join_paths(blis_path, 'lib')
  endif
  
  blis_dep = declare_dependency(
    include_directories: inc_dir,
    link_args: [join_paths(lib_dir, 'libblis.a'), '-pthread', '-lm'], 
    dependencies: [dependency('openmp')]
  )
endif
  
subdir('src')
subdir('tests')
