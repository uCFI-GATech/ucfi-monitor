#!/usr/bin/env python

import sys
import os
import subprocess

elf_file_name = sys.argv[1]
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

    offset_int = int(vectors[1], 16)
    assert (offset_int != 0)
    offset = hex(offset_int)
    funcName = vectors[7]
    print "D", offset, funcName
