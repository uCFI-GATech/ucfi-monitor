#ifndef __UCFI_COMMON_H__
#define __UCFI_COMMON_H__

// llvm headers
#include "llvm/Pass.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/TypeFinder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/IR/GetElementPtrTypeIterator.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Target/TargetLibraryInfo.h"

// help functions 
#include <set>
#include <ctime>
#include <stack>
#include <chrono>
#include <limits>
#include <string>
#include <vector>
#include <cstdlib>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <iostream>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/shm.h>
#include <sys/ipc.h>
#include <pthread.h>
#include <sys/stat.h>
#include <sys/ptrace.h>
#include <map>
#include <unordered_map>
#include <unordered_set>

using namespace llvm;
using namespace std;

namespace PTCFI {}

#endif
