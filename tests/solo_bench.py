import matplotlib.pyplot as plt
from pathlib import Path
import subprocess, sys, os, argparse
from getPath import getPath

def solo_bench(name: str, init_n: int, final_n: int, inc: int = 48, reps: int = 3,
               trans: str = "N", debug: bool = False) -> None:
    """
    --------------------------------------------------
    Input
    --------------------------------------------------
    name: Name of BLAS operation
    init_n: Initial number of columns of (square) matrices in test
    final_n: Final number of columns of (square) matrices in test
    inc: Increment to number of columns in matrices between each trial
    reps: Number of repetitions over which to record maximum performance
    trans: Flags matrices should be transposed in calculation (applies to all matrices)
    debug: Enables debugging output
    --------------------------------------------------
    Output
    --------------------------------------------------
    Plots BLAS operation in Gflops/sec against size of square matrices. 
    """
    
    try:
        executable = getPath(name + "_solo_test")
    except FileNotFoundError as err:
        print(f"Fatal error: {err}", file = sys.stderr)
        raise

    command = [str(executable), str(trans), str(trans), str(init_n), str(final_n), str(inc), str(reps)]

    if debug:
        print(f"Attempting to execute {' '.join(command)}", file = sys.stderr)

    try:
        process = subprocess.run(
            command,
            stdout = subprocess.PIPE,
            text = True,
            check = True)

        output_data = process.stdout.splitlines()
        sqdim = []
        gflops = []

        for line in output_data:
            if line.startswith("Average:"):
                average = float(line.split()[1])
            else:
                n, perf = line.split()
                sqdim.append(int(n))
                gflops.append(float(perf))

    except subprocess.CalledProcessError as err:
        print(f"Error calling {name} with parameters {init_n}, {final_n}, {inc}, {reps}: {err}",
              file = sys.stderr)
        raise

    xAxis = "Matrix dimensions: m = n = p"
    yAxis = "Performance (Gflops/sec)"
    figtitle = "Performance against size of square matrices"
    size = (10,6)

    plt.figure(figsize=size)
    plt.plot(sqdim, gflops, marker = 'o', color = 'b', label = f"{name}")
    plt.xlabel(xAxis)
    plt.ylabel(yAxis)
    plt.title(figtitle)
    plt.grid(True)

    if average is not None:
        plt.annotate(f"Average: {average:.2f} Gflops/sec",
                     xy = (0.5, 0.05), xycoords = 'axes fraction',
                     bbox = dict(boxstyle="round", fc = "w"))
    
    plt.legend()
    plt.show()

if __name__ == "__main__":
    try:
        solo_bench("dgemm", 48, 2400)
    except Exception as err:
        print(f"Fatal error: {err}", file = sys.stderr)
        sys.exit(1)    
