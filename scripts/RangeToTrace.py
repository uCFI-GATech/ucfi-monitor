#!/usr/bin/env python

import sys
import os
import subprocess

elf_file_name = sys.argv[1]
start_addr = 0
end_addr = 0
symbols = subprocess.check_output(["readelf", "-s", elf_file_name, "--wide"]).split("\n")
for symbol in symbols:
    isIFUNC = False
    vectors = symbol.split()
    if len(vectors) < 7:
        continue
    if "FUNC" not in vectors[3] or vectors[6] == "UND":
        continue
    if vectors[3] == "IFUNC":
        print "found IFUNC in binary"
        sys.exit(-1)

    funcName = vectors[7]

    if funcName == "indirect_call":
        start_addr = int(vectors[1], 16)
    elif funcName == "ptwrite":
        ptwrite_chunk_start = int(vectors[1], 16)
        length = int(vectors[2], 16)
        end_addr = ptwrite_chunk_start + length


print "echo -n " +  str(hex(start_addr)) + "\|" +  str(hex(end_addr)) + "\|" + elf_file_name \
        + " | sudo tee /sys/kernel/debug/pt_monitor"
