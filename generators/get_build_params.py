import os
import glob
import math
from cpu_info_tables import AMD_MAP, INTEL_MAP, TARGET_ISA, VFMADD_DATA

"""
Queries the user's CPU data and returns suggested cache parameters for GEMM implementation
Estimates of optimal cache parameters based on Low, et al. (2016)

"""

class CPUSupportError(Exception):
    """Exception raised when CPU lacks necessary features (e.g. AVX2 support)"""
    pass


def get_cache_details() -> dict[dict[int, int, int, int]]:
    """
    Collects cache size and instance count from /sys/devices/system/cpu/cpu0/cache/.
    Returns a dict mapping cache level (e.g., 'L1d', 'L2', 'L3') to dicts with size per unit,
    units, total size, and associativity degree
    """
    cache_info = {}

    cpu_count = os.cpu_count() or 1
    path_prefix = "/sys/devices/system/cpu/cpu0/cache"

    for index_path in glob.glob(os.path.join(path_prefix, "index*")):
        try:
            with open(os.path.join(index_path, "level")) as f:
                level = f.read().strip()
            with open(os.path.join(index_path, "type")) as f:
                cache_type = f.read().strip().lower()
            with open(os.path.join(index_path, "size")) as f:
                size = f.read().strip()
            with open(os.path.join(index_path, "shared_cpu_list")) as f:
                shared_cpu_list = f.read().strip()
            with open(os.path.join(index_path, "ways_of_associativity")) as f:
                ways = int(f.read().strip())
        except FileNotFoundError:
            continue

        size_kb = (int(size[:-1]) * (1024 if size[-1] == "M" else 1) \
                   if size[-1] in "KM" else int(size))

        num_shared = sum((int(r.split("-")[1]) - int(r.split("-")[0]) + 1 if "-" in r else 1)
            for r in shared_cpu_list.split(","))

        units = cpu_count // num_shared if num_shared else 1

        label = f"L{level}{'i' if cache_type == 'instruction' else 'd' if cache_type == 'data' else ''}"
        if label not in cache_info:
            cache_info[label] = { "size": size_kb, "units": units, "total": size_kb * units, "associativity": ways }

    return cache_info


def get_cpu_data() -> dict[str, str, dict[dict[int, int, int, int]]]:
    """
    Gets CPU data from /proc/cpuinfo and returns dictionary of CPU model, AVX support (most
    recent AVX support starting from AVX2), and dictionary of cache data
    """

    cpu_data = {}
    try:
        with open("/proc/cpuinfo") as f:
            for line in f:
                if line.startswith("model name"):
                    cpu_data['model'] = line.split(":", 1)[1].strip()
                if line.startswith("flags"):
                    flags = line.split(":", 1)[1].split()
                    if 'avx512f' in flags:
                        cpu_data['avx'] = 'avx512'
                    elif 'avx2' in flags:
                        cpu_data['avx'] = 'avx2'
    except FileNotFoundError as err:
        raise FileNotFoundError("Could not get CPU data") from err

    if not cpu_data['avx']:
        raise CPUSupportError("AVX2 support required, but not found in CPU flags")
    if not cpu_data['model']:
        raise RuntimeError("Unexpected format in proc/cpuinfo: 'model name' not found")

    cpu_data['cache'] = get_cache_details()
    cpu_data['register_size'] = 512 if (cpu_data['avx'] == 'avx512') else 256
    cpu_data['fma'] = get_fma_data(model_to_arch(cpu_data['model']))
    
    return cpu_data
    
    
def get_fma_data(cpu_arch: str) -> dict[int]:
    """
    Return VFMADD data (latency, throughput)
    """    
    arch = cpu_arch.lower()

    for vendor, cpus in VFMADD_DATA.items():
        for name, data in cpus.items():
            if arch in name:
                return data

    raise CPUSupportError(f"ISA details for {cpu_arch} not found.\n"
                          f"Currently supported architectures:\n \
                          {[cpu for vendor in VFMADD_DATA.values() for cpu in vendor]}")


def model_to_arch(model: str) -> str:
    """
    Takes CPU model name and returns microarchitecture, attempting partial matching
    """
    model = model.lower()
    if "xeon e3" in model:
        raise NotImplementedError("Xeon E3 needs special handling -- Skylake but no AVX512")

    if "amd" in model:
        for prefix, arch in AMD_MAP.items():
            if prefix in model:
                return arch
    elif "intel" in model:
        for prefix, arch in INTEL_MAP.items():
            if prefix in model:
                return arch
        
    raise CPUSupportError(f"CPU microarchitecture not found for {model}.")


def gemm_build_parameters() -> dict[str, dict[int]]:
    """
    Estimates optimal GEMM build parameters based on Low, et al. (2016) -- may not be exactly optimal
    """
    
    build_params = {"sgemm": {}, "dgemm": {}}
    cpu_data = get_cpu_data()
    data_size_B = {"single": 4, "double": 8} # in bytes to match standard cache information
    
    for precision in data_size_B:
        name = precision[0] + "gemm"
        data_size = data_size_B[precision]
        n_per_register = cpu_data['register_size'] // (8 * data_size)
        
        fma_latency, fma_throughput = cpu_data['fma']['latency'], cpu_data['fma']['throughput']
        assoc_degree_1 = cpu_data['cache']['L1d']['associativity']

        # Estimate best microtile dimensions
        build_params[name]['MR'] = math.ceil(math.sqrt(fma_latency * fma_throughput /n_per_register)) * n_per_register
        build_params[name]['NR'] = math.ceil(fma_latency * fma_throughput * n_per_register / build_params[name]['MR'])

        # Estimate best cache parameters
        cache_mp_lines = (assoc_degree_1 - 1)//(1 + build_params[name]['NR']/build_params[name]['MR'])
        build_params[name]['PC'] = int(cache_mp_lines * (cpu_data['cache']['L1d']['size'] * 1024/assoc_degree_1) / (build_params[name]['MR'] * data_size))
    
    return build_params


def print_cpu_info() -> None:
    cache_info = get_cache_details()
    cpu_data = get_cpu_data()

    print("Model:", cpu_data['model'])

    print("Cache details:")
    for level in ["L1i", "L1d", "L2", "L3"]:
        if level in cache_info:
            entry = cache_info[level]
            print(f" {level}: {entry['units']} × {entry['size']} KB, {entry['associativity']}-way associative")

    print(f"AVX instructions: {cpu_data['avx']}")
    

if __name__ == "__main__":
    print_cpu_info()
    print("Suggested GEMM build parameters for CPU:\n")
    bp = gemm_build_parameters()
    for key, value in bp.items():
        print(f"{key}: {value}")
