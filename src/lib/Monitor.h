#ifndef __KB3_MONITOR__
#define __KB3_MONITOR__

#include "common.h"

#include "NodeFactory.h"
#include "PTA.h"

using namespace llvm;
using namespace std;

// necessary inputs
static cl::opt<std::string> BitcodeFile(cl::Positional, cl::desc("[The bitcode file]"), cl::Required);
static cl::opt<std::string> BBInfoFile(cl::Positional, cl::desc("[The BB Info file]"), cl::Required);
static cl::opt<std::string> PinTraceFile(cl::Positional, cl::desc("[The Pin Trace file]"), cl::Required);

#endif
