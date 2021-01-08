#!/usr/bin/env python3

import sys
from subprocess import Popen, PIPE

import os
from pprint import pprint

def get_makefiles(path):
    p = Popen(["find", path, "-type", "f", "-name", "Makefile"], stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate()
    clean = stdout.decode().replace("'", "")
    return clean.split()

KERNEL_PATH = "/usr/src/linux/"
MAKE_PATHS = get_makefiles(KERNEL_PATH)

print(f"KERNEL_PATH: {KERNEL_PATH}")
#pprint(f"MAKEFILE_PATH: {MAKE_PATHS}")

if sys.stdin.isatty():
    print("Enter modules or q to quit")

for line in sys.stdin:
    line = line.lower().strip()
    if ".ko" in line:
        real_name = "+= " + os.path.splitext(line)[0] + ".o"
        p = Popen(["grep", real_name, *MAKE_PATHS], stdout=PIPE, stderr=PIPE)
        stdout, stderr = p.communicate()
        print(stdout.decode())
    elif sys.stdin.isatty():
        if line.strip() == "q":
            exit(0)
        
