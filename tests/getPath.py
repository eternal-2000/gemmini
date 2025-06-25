"""
Gets path to executable. Assumes location of executable is in ../builddir/src
"""

from pathlib import Path
import subprocess, sys, os, argparse

def getPath(name: str, debug: bool = False) -> Path:
    root_dir = Path(__file__).parent.parent
    build_dir = root_dir / "builddir/src"
    executable = build_dir / name

    if debug:
        print(f"Looking for executable in {executable}", file = sys.stderr)

        if not build_dir.exists():
            raise FileNotFoundError(f"Build directory not found: {build_dir}")

        if not executable.exists():
            raise FileNotFoundError(f"Executable {name} not found in {build_dir}")

        if not os.access(executable, os.X_OK):
            raise PermissionError(f"File not executable: {executable}")
    
    return executable
