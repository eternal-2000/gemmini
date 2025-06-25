TARGET_ISA = ['avx2', 'avx512']

VFMADD_DATA = { # Some example CPUs - to be updated later
    "intel": {
        "skylake": {"latency": 4, "throughput": 2}, # But not Xeon E3! For some reason
        "haswell": {"latency": 5, "throughput": 2},
        "ice lake": {"latency": 4, "throughput": 2},
        "tiger lake": {"latency": 4, "throughput": 2},
    },
    "amd": {
        "zen 2": {"latency": 5, "throughput": 2},
        "zen 3": {"latency": 4, "throughput": 2},
        "zen 4": {"latency": 4, "throughput": 2},
    },
}

AMD_MAP = {
    "5950": "Zen 3",
    "5900": "Zen 3",
    "5800": "Zen 3",
    "5700": "Zen 3",
    "5600": "Zen 3",
    "5500": "Zen 3",

        "3950": "Zen 2",
        "3900": "Zen 2",
        "3800": "Zen 2",
        "3700": "Zen 2",
        "3600": "Zen 2",
}

INTEL_MAP = {
    "13900": "Raptor Lake",
    "13700": "Raptor Lake",
    "13600": "Raptor Lake",

        "12900": "Alder Lake",
        "12700": "Alder Lake",
        "12600": "Alder Lake",

        "11900": "Rocket Lake",
        "11700": "Rocket Lake",

        "10900": "Comet Lake",
        "10700": "Comet Lake",

        "9900": "Coffee Lake Refresh",
        "9700": "Coffee Lake Refresh",
        "8700": "Coffee Lake",
        "8600": "Coffee Lake",

        "7700": "Kaby Lake",
        "7600": "Kaby Lake",

        "6900": "Skylake",
        "6700": "Skylake",

        "5960": "Haswell-E",
        "5930": "Haswell-E",
        "5820": "Haswell-E",

        "4790": "Haswell Refresh",
        "4770": "Haswell",
        "4670": "Haswell",
}
