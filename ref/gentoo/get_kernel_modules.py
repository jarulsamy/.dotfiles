#!/usr/bin/env python3

import os
import sys
from subprocess import PIPE, Popen


def get_makefiles(path):
    """Find all the files named `Makefile` within the path specified."""
    p = Popen(
        ["find", path, "-type", "f", "-name", "Makefile"], stdout=PIPE, stderr=PIPE
    )
    stdout, stderr = p.communicate()
    clean = stdout.decode().replace("'", "")
    return clean.split()


KERNEL_PATH = "/usr/src/linux/"
MAKE_PATHS = get_makefiles(KERNEL_PATH)

print(f"KERNEL_PATH: {KERNEL_PATH}")
# print(f"MAKEFILE_PATH: {MAKE_PATHS}")

# Either pipe into stdin in type in module names
if sys.stdin.isatty():
    print("Enter modules or q to quit")

for line in sys.stdin:
    line = line.lower().strip()
    # Replace the file extension to that used in makefiles
    if ".ko" in line:
        real_name = "+= " + os.path.splitext(line)[0] + ".o"
        p = Popen(["grep", real_name, *MAKE_PATHS], stdout=PIPE, stderr=PIPE)
        stdout, stderr = p.communicate()
        print(stdout.decode())
    elif sys.stdin.isatty():
        if line.strip() == "q":
            exit(0)
