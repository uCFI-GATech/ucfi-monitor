#!/user/bin/env python

import os
import sys

if len(sys.argv) != 4:
    print "Usage: " + sys.argv[0] + " file start_line_no end_line_no"
    sys.exit(-1)

file_name = sys.argv[1]
start_line_no = int(sys.argv[2])
end_line_no = int(sys.argv[3])

with open(file_name, "r") as f:
    lines = f.readlines()
    total_line_no = len(lines)
    if start_line_no > total_line_no:
        print "start_line_no " + str(start_line_no) + \
                " > total line_no " + str(total_line_no)
        start_line_no = 1

    if (start_line_no > end_line_no):
        print "start_line_no " + str(start_line_no) + \
                " > end_line_no " + str(end_line_no)
        sys.exit(-1)

    if end_line_no > total_line_no:
        print "end_line_no " + str(end_line_no) + \
                " > total_line_no " + str(total_line_no)
        end_line_no = total_line_no

    callStack = []

    linesToCheck = lines[start_line_no - 1 : end_line_no]
    line_no = start_line_no
    for line in linesToCheck:
        line_no += 1
        if line.startswith("push") or line.startswith("indirect push"):
            funcName = line[line.find("push") + 5:].split(" ")[0].strip()
            print "%5s" % str(line_no), "\tpush " + funcName
            callStack.append(funcName)
        elif "\tpop" in line and "until" not in line:
            funcName = line[line.find("pop") + 4:].split(" ")[0].strip()
            print "%5s" % str(line_no), "\tpop  " + funcName
            expectedFuncName = callStack.pop()
            if expectedFuncName != funcName:
                print "\nExpect", expectedFuncName + " != " + funcName 
                print "\nCallStack:\n"
                callStack.append(expectedFuncName)
                callStack.reverse()
                stackDepth = len(callStack)
                for func in callStack:
                    print "\t", "%3s" % stackDepth, "\t", func
                    stackDepth -= 1
                sys.exit(-1)

    print "\nCongrats! your call stack is good"
