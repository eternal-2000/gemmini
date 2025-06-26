python3 = find_program('python3')
plot_perf = files('solo_bench.py')

custom_target('plot_performance',
              command: [python3, plot_perf],
              output: 'plotperf.done',
              build_by_default: false,
              install: false
             )
