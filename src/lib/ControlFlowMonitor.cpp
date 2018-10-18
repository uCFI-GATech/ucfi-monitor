#include "common.h"

#include <unistd.h>
#include <ctime>
#include <functional>
#include "pt.h"
#include "PTA.h"
#include "PTTrace.h"
#include "KB3Instruction.h"
#include "ExternalLibrary.h"
#include "ControlFlowMonitor.h"

#define MAX_RECORD_NUM  (1 << 16) // 16 best
#define MAX_PT_DATA_BUF (1 << 24)
#define MAX_RECORD_LIMIT (MAX_RECORD_NUM - 6)

#define COMMA ,

namespace PTCFI {
	// used for following forks()
	bool  NeedFork = 0;
	pid_t ChildPID = 0;

	// output logs
	raw_fd_ostream * ofs;
	raw_fd_ostream * ofs2;

	// global trace file name
	string PinTraceFileName;
	FILE * PinTraceFile;

	// packet counter
	DR(uint64_t global_counter);

	DS(DenseMap<uint64_t COMMA uint64_t> BB_statistics);
	DS(std::map<uint64_t COMMA uint64_t> BB_stat_map);
	DS(uint64_t BB_total  = 0);
	DS(uint64_t ret_total = 0);
	DS(uint64_t idx_total = 0);
	DS(uint64_t cnd_total = 0);

	uint64_t BB_packet = 0;

	// to remember decision between Griffin packets
	bool logPGD = false;
	bool expectTIP = false;

	// address of ptwrite_chunk
	// the *_tag, *_upper is for optimization
	uint64_t ptwrite_chunk_addr = 0;
	uint16_t ptwrite_chunk_tag = 0;
	uint64_t ptwrite_chunk_upper = 0;
	uint64_t ptwrite_addr = 0;
	uint16_t ptwrite_tag = 0;
	uint64_t aRet_addr = 0;
	uint16_t aRet_tag = 0;
	uint64_t indirect_call_addr = 0;
	uint16_t indirect_call_tag = 0;

	// map each BB from ID to its KB3Instrs
	KB3Instruction *                BBID2KB3Instrs[MAX_BB_NUM];

	// sensitive functions
	unordered_set<Function *>       SensitiveFuncs;
	// Address taken functions
	unordered_set<Function *>       AT_Functions;
	DenseMap<Function *, uint64_t>  AT_Func2Addr;
	DenseMap<uint64_t, Function *>  AT_Addr2Func;
	// All functions
	unordered_map<string, uint64_t> AllFuncName2Addr;
	DenseMap<uint64_t, Function*>   AllAddr2Func;

	// all KB3CallInvokeInstructions
	unordered_set<KB3CallInvokeInstruction *> AllKB3CallInvokeInstrs;

	// for purify
	typedef DenseMap<Value*, unordered_set<Function*> > IndirectCallMap;
	IndirectCallMap       AllIndirectCalls;
	unordered_set<Value*> AllUndefinedHistory;
	unordered_set<Value*> AllUndefinedNormalValues;
	unordered_set<Value*> AllUndefinedValueArgument;

	// for trace reading
	uint8_t      PTDataBuf[MAX_PT_DATA_BUF];
	uint8_t *    PTBufCheckpoint = 0;
	size_t       PTBufCheckpointSize = 0;
	pid_t        PTBufCheckpointTid = 0;
	uint64_t     packageNumA, packageNumB;
	uint64_t *   currNum = &packageNumA;
	RecordType   recordArrayA[MAX_RECORD_NUM];
	RecordType   recordArrayB[MAX_RECORD_NUM];
	RecordType * currArray = recordArrayA;
	RecordType * currArrayEnd;
	RecordType * nextPacket = recordArrayA;
	pthread_t    fetch_thread;

	// locks
	pthread_mutex_t parserInArrayA;
	pthread_mutex_t parserInArrayB;
	pthread_mutex_t readerInArrayA;
	pthread_mutex_t readerInArrayB;

	unsigned long    getIDFromBB(BasicBlock * BB);
	vector<string>   tokenize(string line, char delim);
	KB3Instruction * addKB3Instruction(Instruction * I);
	KB3Instruction * addKB3Instruction(BasicBlock * BB);
	Function *       getFuncFromName(Module * M, string funcName);
	int  parsePTBuf(unsigned char *buf, size_t len, pid_t tid,
			RecordType *recordArray, uint64_t *packageNum);
	void getNextPacketBuf(RecordType *RecordArray, uint64_t *packageNum, 
			pthread_mutex_t *lock, FILE *PinTraceFile);
	// for purify module
	bool PurifyArgument(Module * M);
	void AnalyzeIndirectCalls(Module &M);
	void handleCallSite(ImmutableCallSite &CS, Function *F);
	bool isTypeMatch(ImmutableCallSite CS, Function *F, Type *ReturnType);

	void *fetchBuf(void *)
	{
		while (1) {
			pthread_mutex_lock(&readerInArrayB);
			getNextPacketBuf(recordArrayB, &packageNumB, &parserInArrayB, PinTraceFile);
			pthread_mutex_lock(&readerInArrayA);
			getNextPacketBuf(recordArrayA, &packageNumA, &parserInArrayA, PinTraceFile);
		}
	}

	void followFork(pid_t child)
	{
		NeedFork = 0;
		if (fork()) {
			//llvm_unreachable("stop parent");
			swapBufs();
			return;
		}

		(*ofs) << "===== Forked to follow child " << child << "\n";

		// Close current PT file and open child's PT file
		string filename(PinTraceFileName);
		if (filename.find_last_of("/") != string::npos) {
			filename = filename.substr(filename.find_last_of("/") + 1);

			PinTraceFileName.replace(PinTraceFileName.find_last_of("/") + 1,
					PinTraceFileName.length() - PinTraceFileName.find_last_of("/") + 1,
					string("pt_").append(to_string(child)));
		} else {
			PinTraceFileName = string("pt_").append(to_string(child));
		}
		time_t timeout = time(NULL) + 2;
		do {
			// Busy waiting
		} while (!llvm::sys::fs::exists(PinTraceFileName) && time(NULL) < timeout);
		if (!llvm::sys::fs::exists(PinTraceFileName)) {
			(*ofs) << "PT file \"" << PinTraceFileName << "\" does not exist\n";
			(*ofs).flush();
			exit(1);
		}
		fclose(PinTraceFile);

		// Restart PT reading thread
		initializePinTrace(PinTraceFileName);
	}

	void getNextPacketBuf(RecordType *RecordArray, uint64_t *packageNum, 
			pthread_mutex_t *lock, FILE *PinTraceFile)
	{
		// Fill recordArray with RecordType up to MAX_RECORD_NUM
		struct pt_logitem_header *header = (pt_logitem_header *) PTDataBuf;
		struct pt_logitem_buffer *buffer;
		struct pt_logitem_fork *forkbuf;
		RecordType *ptr = RecordArray;
		*packageNum = 0;

		// Giving the analysis thread an empty buffer will signal that either we've reached
		// the end of the trace or a fork is needed.
		if (NeedFork) {
			pthread_mutex_unlock(lock);
			return;
		}

		// If last time we ended with a PT buffer only partially parsed,
		// resume parsing now
		if (PTBufCheckpoint) {
			if (parsePTBuf(PTBufCheckpoint, PTBufCheckpointSize, PTBufCheckpointTid,
						RecordArray, packageNum)) {
				pthread_mutex_unlock(lock);
				return;
			}
			ptr = RecordArray + *packageNum;
		}

		while (*packageNum < MAX_RECORD_LIMIT) {
			if (!fread(PTDataBuf, 1, sizeof(pt_logitem_header), PinTraceFile))
				break;

			if (header->kind == MAGIC)
				continue;

			/* read in */
			size_t ignore = fread(((uint8_t *)PTDataBuf) + sizeof(pt_logitem_header), 
					1, header->size - sizeof(pt_logitem_header), PinTraceFile);
			(void)ignore;

			switch (header->kind) {
				case PT_LOGITEM_BUFFER:
					buffer = (struct pt_logitem_buffer *) PTDataBuf;
					if (parsePTBuf((unsigned char *)(buffer + 1), buffer->size, 
								buffer->pid, ptr, packageNum)) {
						pthread_mutex_unlock(lock);
						return;
					}
					ptr = RecordArray + *packageNum;
					break;
				case PT_LOGITEM_FORK:
					forkbuf = (struct pt_logitem_fork *) PTDataBuf;
					if (forkbuf->child_pid == ChildPID)
						break; // We've already requested that this fork be followed
					ChildPID = forkbuf->child_pid;
					NeedFork = 1;
					pthread_mutex_unlock(lock);
					return;
				case PT_LOGITEM_THREAD:
				case PT_LOGITEM_PROCESS:
				case PT_LOGITEM_IMAGE:
				case PT_LOGITEM_XPAGE:
				case PT_LOGITEM_UNMAP:
					break;
				default:
					(*ofs) << "Unexpected packet type " << HEX(header->kind) << "\n";
					break;
			}
		}

		pthread_mutex_unlock(lock);
	}

	int parsePTBuf(unsigned char *buf, size_t len, pid_t tid,
			RecordType *recordArray, uint64_t *packageNum)
	{
		unsigned char *ptr = buf;
		const unsigned char *end = buf + len;
		uint64_t num = 0;
		const uint64_t num_limit = MAX_RECORD_LIMIT - *packageNum;
		size_t remaining = len;
		unsigned long packet_len;
		unsigned char mask;
		unsigned char bit_selector;
		uint16_t tag;
		RecordType *record;

		bool logPGD0 = logPGD;
		bool expectTIP0 = expectTIP;
		uint64_t ptwrite_chunk_end = ptwrite_chunk_addr + (1 << 22);

		DP(static uint64_t counter = 0);

		record = recordArray;

		while (ptr < end) {
			if (num >= num_limit) {
				// There might not be enough space left to parse the next PT packet,
				// save our current position and inform caller to resume later.
				PTBufCheckpoint = ptr;
				PTBufCheckpointSize = remaining;
				PTBufCheckpointTid = tid;
				*packageNum += num;
				logPGD = logPGD0;
				expectTIP = expectTIP0;
				return 1;
			}

			switch (pt_get_packet(ptr, remaining, &packet_len)) {
				case PT_PACKET_OVF:
					DP((*ofs2) << "OVF \n");
					DP(ofs2->flush());

					break;
				case PT_PACKET_TIPPGE:
					/* Optimization - Due to the layout of our inserted PT write chunk, the two least
					 * significant bytes always differ for aRet, indirect call, and ptwrite.
					 */
					tag = *(uint16_t *)(ptr + 1);
					if ((tag == aRet_tag) || (tag == indirect_call_tag)) {
						logPGD0 = true;
					} else if (tag == ptwrite_tag)
						expectTIP0 = true;

					DP((*ofs2) << "PGE: " << HEX(tag) << " - " << counter << "\n");
					DP(ofs2->flush());

					break;
				case PT_PACKET_TIPPGD:
					if (packet_len <= 1) {
						break;
					} else if (expectTIP0) {
						/* Optimization - Since we're only tracing a very small portion of the program,
						 * we can take shortcuts to avoid having to decompress the entire target IP.
						 * For TIP packets, if the compressed address is 2 bytes (i.e. packet length is
						 * 3 bytes) we only have to subtract the corresponding 2 bytes of the PT write
						 * chunk address. Otherwise, the compressed address should be 4 bytes and we do
						 * a 32-bit subtraction, taking advantage of the base address never being above
						 * 0xFFFFFFFF.
						 */
						if (packet_len == 3)
							record->addr = *(uint16_t *)(ptr + 1) - ptwrite_chunk_tag;
						else
							record->addr = *(uint32_t *)(ptr + 1) - ptwrite_chunk_addr;

						DP(counter++);
						DP((*ofs2) << "TIP: " << HEX(record->addr) << " - " << counter << "\n");
						DP((*ofs2) << "packet_len: " << packet_len << "\n");
						DP(ofs2->flush());

						num++;
						record++;
						expectTIP0 = false;
						break;
					} else if (logPGD0) {
						/* Optimization - Because of where we place the PT write chunk, a TIP.PGD will
						 * rarely share non-zero most significant bytes with the previous address, so we
						 * don't need to decompress in most cases.
						 */

						if (packet_len > 3)
							record->addr = *(uint32_t *)(ptr + 1);
						else // Fallback for rare cases when PT did compress part of the address
							record->addr = *(uint16_t *)(ptr + 1) | ptwrite_chunk_upper;

						DP(counter++);
						DP((*ofs2) << "PGD: " << HEX(record->addr) << " - " << counter << "\n");
						DP((*ofs2) << "packet_len: " << packet_len << "\n");
						DP(ofs2->flush());

						num++;
						record++;
						logPGD0 = false;
					} else {
						DP((*ofs2) << "PGD\n");
						DP((*ofs2) << HEX(*(uint32_t *)(ptr + 1)) << "\n");
						DP(ofs2->flush());
					}
					break;
				default:
					break;
			}
			ptr += packet_len;
			remaining -= packet_len;
		}
		PTBufCheckpoint = 0;
		PTBufCheckpointSize = 0;
		PTBufCheckpointTid = 0;
		*packageNum += num;
		logPGD = logPGD0;
		expectTIP = expectTIP0;
		return 0;
	}

	void initializePinTrace(string _PinTraceFileName)
	{
		// Get current working directory as a CPP string
		char *cwd_cstr = getcwd(NULL, 0);
		string cwd = string(cwd_cstr);
		cwd += "/";
		free(cwd_cstr);

		PinTraceFileName = string(_PinTraceFileName);

		// Get just the filename from the provided path
		string filename(PinTraceFileName);
		if (filename.find_last_of("/") != string::npos)
			filename = filename.substr(filename.find_last_of("/") + 1);

		std::error_code EC;
		// Open debugging logfiles in current working directory
		ofs = new raw_fd_ostream(cwd + filename + "-log.1", EC, sys::fs::F_RW | sys::fs::F_Text);
		ofs2 = new raw_fd_ostream(cwd + filename + "-log.2", EC, sys::fs::F_RW | sys::fs::F_Text);

		DS(BB_statistics.clear());
		DS( BB_total = 0);
		DS(ret_total = 0);
		DS(idx_total = 0);
		DS(cnd_total = 0);
		DS(PTA::clean_count());

		if (!llvm::sys::fs::exists(PinTraceFileName)) {
			(*ofs) << "PT file \"" << PinTraceFileName << "\" does not exist\n";
			return;
		}
		PinTraceFile = fopen(PinTraceFileName.c_str(), "r");

		if (!filename.compare("pt_output")) {
			// pt_output is special. It is not a PT trace, but rather 
			// tells us the name of the file that contains the first 
			// spawned thread of what we want to trace.
			pid_t root_pid;
			size_t ignore = fread(&root_pid, sizeof(pid_t), 1, PinTraceFile);
			(void) ignore;
			if (root_pid < 1) {
				(*ofs) << "Failed to get root PID from pt_output\n";
				return;
			}
			// The real PT trace filename is in the same dir and called 
			// "pt_<root_pid>"
			PinTraceFileName.replace(PinTraceFileName.end() - 9, 
					PinTraceFileName.end(), 
					string("pt_").append(to_string(root_pid)));
			if (!llvm::sys::fs::exists(PinTraceFileName)) {
				(*ofs) << "PT file \"" << PinTraceFileName << "\" does not exist\n";
				return;
			}
			fclose(PinTraceFile);
			PinTraceFile = fopen(PinTraceFileName.c_str(), "r");
		}

		NeedFork = 0;
		currArray = recordArrayA;
		nextPacket = recordArrayA;
		currNum = &packageNumA;

		pthread_mutex_init(&parserInArrayA, NULL);
		pthread_mutex_init(&parserInArrayB, NULL);
		pthread_mutex_init(&readerInArrayA, NULL);
		pthread_mutex_init(&readerInArrayB, NULL);

		getNextPacketBuf(currArray,currNum, &parserInArrayA, PinTraceFile);
		currArrayEnd = currArray + *currNum;
		pthread_mutex_lock(&parserInArrayA);
		pthread_mutex_lock(&parserInArrayB);
		pthread_mutex_lock(&readerInArrayA);
		pthread_create(&fetch_thread, NULL, fetchBuf, nullptr);

		return;
	}

	void swapBufs()
	{
		if (currArray == recordArrayA)
		{
			pthread_mutex_lock(&parserInArrayB);
			pthread_mutex_unlock(&readerInArrayA);
			currArray = recordArrayB;
			nextPacket = recordArrayB;
			currNum = &packageNumB;
			currArrayEnd = recordArrayB + packageNumB;
		} else {
			pthread_mutex_lock(&parserInArrayA);
			pthread_mutex_unlock(&readerInArrayB);
			currArray = recordArrayA;
			nextPacket = recordArrayA;
			currNum = &packageNumA;
			currArrayEnd = recordArrayA + packageNumA;
		}

		if(!(*currNum))
		{
			if (NeedFork)
			{
				followFork(ChildPID);
			} else {
				(*ofs) << "End of trace\n";
				ofs->flush();
				errs() << "End of trace: " << PinTraceFileName << "\n";

				DS(for (auto iter : BB_statistics))
					DS( BB_stat_map[iter.first] = iter.second);
				//typedef std::function<bool(std::pair<uint64_t, uint64_t>, std::pair<uint64_t, uint64_t>)> Comparator;
				//Comparator compFunctor =
				//    [](std::pair<uint64_t, uint64_t> elem1 ,std::pair<uint64_t, uint64_t> elem2)
				//    { return elem1.second < elem2.second; };
				//std::set<std::pair<uint64_t, uint64_t>, Comparator> setOfWords(
				//    BB_stat_map.begin(), BB_stat_map.end(), compFunctor);
				//DS(for (auto iter : setOfWords))
				//DS(	(*ofs) << format("%6d", iter.first) << " : " << iter.second << "\n");
				DS((*ofs) << "\n");
				DS((*ofs) << "TOTAL BB  COUNT: " <<  BB_total << "\n");
				DS((*ofs) << "TOTAL RET COUNT: " << ret_total << "\n");
				DS((*ofs) << "TOTAL IDX COUNT: " << idx_total << "\n");
				DS((*ofs) << "TOTAL CND COUNT: " << cnd_total << "\n");
				DS(PTA::dump_call_count());
				DS(ofs->flush());

				exit(0);
			}
		}
	}

	void saveModule2(Module * M, Twine filename)
	{
		int bc_fd;
		sys::fs::openFileForWrite(filename + "_pt.bc", bc_fd, 
				sys::fs::F_RW | sys::fs::F_Text);
		raw_fd_ostream bc_file(bc_fd, true, true);
		WriteBitcodeToFile(M, bc_file);
	}

	void SensitiveFuncAnalysis(Module * M)
	{
		SensitiveFuncs.clear();

		for (Module::iterator it = M->begin(), ie = M->end(); 
				it != ie; ++it) {
			Function &f = *it;
			if (f.isDeclaration() || f.isIntrinsic())
				continue;

			if (f.hasFnAttribute("sensitive-func"))
				SensitiveFuncs.insert(&f);
		}
	}

	bool isTypeMatch(ImmutableCallSite CS, Function *F, Type *ReturnType) {
		Function::const_arg_iterator fItr = F->arg_begin();
		ImmutableCallSite::arg_iterator aItr = CS.arg_begin();

		if (ReturnType != F->getReturnType())
			return false;

		while (fItr != F->arg_end() && aItr != CS.arg_end()) {
			Argument *formal = const_cast<Argument*> (&(*fItr));
			Value *actual = *aItr;
			if (formal->getType() != actual->getType())
				return false;
			++fItr;
			++aItr;
		}

		if (fItr == F->arg_end() && aItr == CS.arg_end())
			return true;

		return false;
	}

	void AnalyzeIndirectCalls(Module &M) 
	{
		for (Module::iterator it = M.begin(), ie = M.end(); 
				it != ie; ++it) {
			Function &f = *it;
			if (f.isDeclaration() || f.isIntrinsic())
				continue;

			for (inst_iterator ii = inst_begin(f), ie = inst_end(f);
					ii != ie; ++ii) {
				Instruction *inst = &(*ii);
				if (CallInst *cInst = dyn_cast<CallInst>(inst)) {
					if(!(cInst->getCalledFunction())) {
						std::unordered_set<Function*> &targets = 
							AllIndirectCalls[cInst->getCalledValue()];
						for (Function *tmpF : AT_Functions)
							if (isTypeMatch(ImmutableCallSite(cInst), 
										tmpF, inst->getType()))
								targets.insert(tmpF);
					}
				} else if (InvokeInst *iInst = dyn_cast<InvokeInst>(inst)) {
					if (!(iInst->getCalledFunction())) {
						std::unordered_set<Function*> &targets = 
							AllIndirectCalls[iInst->getCalledValue()];
						for (Function *tmpF : AT_Functions)
							if (isTypeMatch(ImmutableCallSite(iInst), 
										tmpF, inst->getType()))
								targets.insert(tmpF);
					}
				}
			}
		}
	}

	bool PurifyArgument(Module * M) {
		AllUndefinedValueArgument.clear();
		for (Module::iterator it = M->begin(), ie = M->end(); 
				it != ie; ++it) {
			Function &f = *it;

			if (f.isDeclaration() || f.isIntrinsic())
				continue;

			for (inst_iterator ii = inst_begin(f), ie = inst_end(f);
					ii != ie; ++ii) {
				Instruction *inst = &(*ii);

				if (CallInst *cInst = dyn_cast<CallInst>(inst)) {
					ImmutableCallSite cs(cInst);
					Function *calledFunction = cInst->getCalledFunction();
					if (calledFunction) {
						handleCallSite(cs, calledFunction);
					} else {
						Value *calledValue = cInst->getCalledValue();
						std::unordered_set<Function*> &targets = 
							AllIndirectCalls[calledValue];
						for (Function *target : targets) {
							handleCallSite(cs, target);
						}
					}
				} else if (InvokeInst *iInst = dyn_cast<InvokeInst>(inst)) {
					ImmutableCallSite cs(iInst);
					Function *calledFunction = iInst->getCalledFunction();
					if (calledFunction) {
						handleCallSite(cs, calledFunction);
					} else {
						Value *calledValue = iInst->getCalledValue();
						std::unordered_set<Function*> &targets = 
							AllIndirectCalls[calledValue];
						for (Function *target : targets) {
							handleCallSite(cs, target);
						}
					}
				}
			}
		}

		return !AllUndefinedValueArgument.empty();
	}

	void handleCallSite(ImmutableCallSite &CS, Function *F) {
		Function::const_arg_iterator fItr = F->arg_begin();
		ImmutableCallSite::arg_iterator aItr = CS.arg_begin();

		while (fItr != F->arg_end() && aItr != CS.arg_end()) {
			Argument *formal = const_cast<Argument*> (&(*fItr));
			Value *actual = *aItr;
			if (isa<UndefValue>(actual) && !isa<UndefValue>(formal)) {
				if (AllUndefinedHistory.find(formal) == AllUndefinedHistory.end()) {
					AllUndefinedValueArgument.insert(formal);
				}
			}

			++fItr;
			++aItr;
		}
	}

	void PurifyModule2(Module * M)
	{
		// collect all address-taken functions and analyze indirect calls	
		for (Module::iterator it = M->begin(), ie = M->end();
				it != ie; ++it) {
			Function &f = *it;
			if (f.hasAddressTaken()) {
				AT_Functions.insert(&f);
			}
		}

		SensitiveFuncAnalysis(M);

		unordered_set<GlobalVariable *> allRemoveGlobals;
		// remove non-interesting global variables
		for (auto& globalVal: M->globals()) {
			const Type *type = globalVal.getType()->getElementType();
			if (const ArrayType *arrayType = dyn_cast<ArrayType>(type)) {
				// skip i8 array
				if (arrayType->getElementType()->getPrimitiveSizeInBits() 
						== 8)
					allRemoveGlobals.insert(&globalVal);
			}
		}
		for (auto G : allRemoveGlobals) {
			G->replaceAllUsesWith(UndefValue::get(G->getType()));
			G->eraseFromParent();
		}

		// keep 3 types of instructions:
		// 		sensitive 
		// 		terminator
		// 		call to ptwrite
		for (auto & F : *M) {
			for (Function::iterator bb = F.begin(), bbEnd = F.end(); 
					bb != bbEnd; ) {
				BasicBlock * BB = &(*bb++);
				for (BasicBlock::iterator i = BB->begin(), iEnd = BB->end(); 
						i != iEnd;) {
					Instruction * I = &(*i++);
					if (I->getMetadata("is-sensitive")) 
						continue;
					if (isa<TerminatorInst>(I))
						continue;
					if (CallInst * CI = dyn_cast<CallInst>(I)) {
						Function * F = CI->getCalledFunction();
						if (F) {
							if (F->getName() == "ptwrite")
								continue;
						} else if (!CI->isInlineAsm()) {
							CI->dump();
							llvm_unreachable("indirect call is not sensitive!!!!!");
						}
					}

					I->replaceAllUsesWith(UndefValue::get(I->getType()));
					I->eraseFromParent();
				}
			}
		}


		AnalyzeIndirectCalls(*M);

		bool changed = true;
		AllUndefinedValueArgument.clear();
		AllUndefinedNormalValues.clear();
		while (changed) {
			changed = false;
			changed |= PurifyArgument(M);

			errs() << "to undefine " << AllUndefinedValueArgument.size() << " arguments\n";
			for (Value *v : AllUndefinedValueArgument) {
				AllUndefinedHistory.insert(v);
				v->replaceAllUsesWith(UndefValue::get(v->getType()));
				//v->eraseFromParent();
			}
		}	

		saveModule2(M, "purified");
	}

	bool hasPHINodeProcessor(BasicBlock * BB)
	{
		bool ret = false;
		const TerminatorInst * TI = BB->getTerminator();
		unsigned SuccNum = TI->getNumSuccessors();
		for (unsigned index = 0; index < SuccNum; index++) {
			BasicBlock * succ = TI->getSuccessor(index);
			if (PHINode * PN = dyn_cast<PHINode>(succ->begin())) {
				ret = true;
				break;
			}
		}

		return ret;
	}

	void dumpBBID2KB3Instrs()
	{
		for (unsigned index = 1; index < MAX_BB_NUM; index++) {
			errs() << index << "--------------\n";
			KB3Instruction * tmp = BBID2KB3Instrs[index];
			if (!tmp) break;
			for (; tmp; tmp = tmp->next)
				if (tmp->inst)
					tmp->inst->dump();
				else
					errs() << "update BB only\n";
		}
	}

	void ProcessModule(Module * M, PTA * pta)
	{
		memset(BBID2KB3Instrs, 0, sizeof(BBID2KB3Instrs));
		for (auto & F : *M) {
			if (F.isDeclaration() || F.isIntrinsic())
				continue;
			pta->createArgumentNodes(&F);
		}

		uint64_t KB3Num = 0;
		for (auto & F : *M) {
			for (Function::iterator bb = F.begin(), bbEnd = F.end(); 
					bb != bbEnd; bb++) {
				BasicBlock * BB = &(*bb);
				unsigned long BBID = getIDFromBB(BB);
				if (!BBID) continue;
				if (BBID > MAX_BB_NUM)
					llvm_unreachable("need to make MAX_BB_NUM larger");
				KB3Instruction *head = nullptr, *cur = nullptr;
				for (BasicBlock::iterator i = BB->begin(), iEnd = BB->end(); 
						i != iEnd; i++) {
					Instruction * I = &(*i);
					if (I->getMetadata("is-sensitive") || 
							((isa<CallInst>(I) || isa<InvokeInst>(I)) && 
							 !I->getMetadata("ptwrite"))) {
						if (isa<BitCastInst>(I))
							continue;
						if (I->hasNUses(0))
							if (!isa<CallInst>(I)   &&
									!isa<InvokeInst>(I) && !isa<StoreInst>(I) &&
									!isa<ReturnInst>(I) && !isa<SelectInst>(I) && 
									!(isa<GetElementPtrInst>(I) && !cast<GetElementPtrInst>(I)->hasAllConstantIndices()))
								continue;

						if (CallInst * CI = dyn_cast<CallInst>(I)) {
							if (CI->isInlineAsm())
								continue;
						}
						KB3Instruction * temp = addKB3Instruction(I);
						KB3Num++;
						temp->next = nullptr;
						if (!head) 
							cur = head = temp;
						else {
							cur->next = temp;
							cur = temp;
						}
					}
				}
				if (hasPHINodeProcessor(BB)) {
					if (!head) {
						KB3Num++;
						cur = head = addKB3Instruction(BB);
					}
					cur->shouldUpdateBB();
				}

				BBID2KB3Instrs[BBID] = head;
			}
		}

		errs() << "got " << KB3Num << " KB3Instructions\n";

		for (auto & kb3CallInvokeInstr : AllKB3CallInvokeInstrs) {
			const Value * calledValue = kb3CallInvokeInstr->calledValue;
			if (!calledValue)
				continue;

			NodeAction * tmp = nullptr;
			if (const Function * func = dyn_cast<Function>(calledValue)) {
				tmp = new FunctionNode(func);
			} else {
				tmp = GeneralNode::getGeneralNode(calledValue, kb3CallInvokeInstr->func);
			}
			if (tmp == nullptr) {
				const Instruction * inst = kb3CallInvokeInstr->inst;
				inst->dump();
				calledValue->dump();
				errs() << inst->getParent()->getName() << "\n";
				errs() << inst->getParent()->getParent()->getName() << "\n";
				llvm_unreachable("null node act, by HH");
			}
			kb3CallInvokeInstr->targetNodeAct = tmp;
		}

		//dumpBBID2KB3Instrs();
	}

	unsigned long getIDFromBB(BasicBlock * BB)
	{
		Instruction * I = BB->getFirstNonPHI();
		CallInst * CI = dyn_cast_or_null<CallInst>(I);
		if (!CI)
			return 0;
		Function * F = CI->getCalledFunction();
		if (F->getName() != "ptwrite")
			return 0;
		Value * arg = CI->getArgOperand(0);
		if (ConstantExpr * CE = dyn_cast<ConstantExpr>(arg)) {
			if (CE->getOpcode() == Instruction::Add) {
				Value * addend = CE->getOperand(1);
				ConstantInt * Addend = cast<ConstantInt>(addend);
				return Addend->getZExtValue();
			} else if (CE->getOpcode() == Instruction::PtrToInt)
				return 0;
		} else
			return 0;
	}

	KB3Instruction * addKB3Instruction(BasicBlock * BB)
	{
		return (new KB3Instruction(BB));
	}

	KB3Instruction * addKB3Instruction(Instruction * I)
	{
		if (!I) {
			errs() << "null instruction\n";
			return nullptr;
		}

		KB3Instruction * k_inst = nullptr;
		if (CallInst * call_inst = dyn_cast<CallInst>(I))
			k_inst = new KB3CallInvokeInstruction(call_inst, nullptr);
		else if (TerminatorInst * term_inst = dyn_cast<TerminatorInst>(I))
		{
			if (ReturnInst * ret_inst = dyn_cast<ReturnInst>(I))
				k_inst = new KB3RetInstruction(ret_inst);
			else if (BranchInst * branch_inst = dyn_cast<BranchInst>(I))
				k_inst = new KB3BranchInstruction(branch_inst);
			else if (InvokeInst * invoke_inst = dyn_cast<InvokeInst>(I))
				k_inst = new KB3CallInvokeInstruction(nullptr, invoke_inst);
		}
		else
			k_inst = new KB3Instruction(I);

		return k_inst;
	}

	vector<string> tokenize(string line, char delim)
	{
		vector<string> strings;
		istringstream f(line);
		string s;
		while (getline(f, s, delim)) {
			strings.push_back(s);
		}

		return strings;
	}

	string IgnoredFunc[] = {
		"deregister_tm_clones",
		"register_tm_clones",
		"__do_global_dtors_aux",
		"frame_dummy",
		"_fini",
		"_start",
		"_init",
		"indirect_call",
		"__libc_csu_init",
		"__libc_csu_fini",
		"aRet",
		"get_stack_high_b",
		"ucfi_init"
	};

	Function * getFuncFromName(Module * M, string funcName)
	{
		Function * func = nullptr;
		GlobalValue * GV = M->getNamedValue(funcName);
		if (GV == nullptr) {
			bool ignored = false;
			for (unsigned index = 0; index < sizeof(IgnoredFunc)/sizeof(IgnoredFunc[0]); index++) {
				if (IgnoredFunc[index].compare(funcName) == 0) {
					ignored = true;
					break;
				}
			}

			if (!ignored && funcName.find("indirect_call_") == 0)
				ignored = true;

			if (!ignored)
				errs() << "failed to get GlobalValue:" << funcName << "\n";
		} else {
			if (Function * f = dyn_cast<Function>(GV))
				func = f;
			else if (GlobalAlias * GA = dyn_cast<GlobalAlias>(GV)) {
				Constant * aliasee = GA->getAliasee();
				if (Function * f = dyn_cast<Function>(aliasee))
					func = f;
				else if (ConstantExpr * CE = dyn_cast<ConstantExpr>(aliasee)) {
					if (CE->getOpcode() == Instruction::BitCast) {
						Value * firstOp = CE->getOperand(0);
						if (Function * f = dyn_cast<Function>(firstOp))
							func = f;
						else
							goto errorlabel;
					} else
						goto errorlabel;
				} else
					goto errorlabel;
			} else
				goto errorlabel;
		}

		return func;

errorlabel:
		GV->dump();
		llvm_unreachable("unknown function");
	}

	void HandleBBInfoFile(string BBInfoFile, Module * M)
	{
		if (!llvm::sys::fs::exists(BBInfoFile)) {
			errs() << "BB info file \"" << BBInfoFile << "\" does not exist\n";
			exit(-1);
		}
		ifstream BBInfo;
		errs() << BBInfoFile << "\n";
		BBInfo.open(BBInfoFile, fstream::in);

		while (!BBInfo.eof())
		{
			string line;
			getline(BBInfo, line);
			vector<string> tokens = tokenize(line, ' ');

			if (tokens.size() == 0)
				continue;

			switch(tokens[0][0]) 
			{
				case 'D': {
										uint64_t addr = stoul(tokens[1], nullptr, 16);
										string funcName = tokens[2];
										Function * func = getFuncFromName(M, funcName);
										AllFuncName2Addr[funcName] = addr;
										AllAddr2Func[addr] = func;

										if (funcName == "ptwrite_chunk") {
											ptwrite_chunk_addr  = addr;
											ptwrite_chunk_tag   = addr & 0xFFFF;
											ptwrite_chunk_upper = addr & 0xFFFFFFFFFFFF0000;
										} else if (funcName == "ptwrite") {
											ptwrite_addr = addr;
											ptwrite_tag  = addr & 0xFFFF;
										} else if (funcName == "aRet") {
											aRet_addr = addr;
											aRet_tag  = addr & 0xFFFF;
										} else if (funcName == "indirect_call") {
											indirect_call_addr = addr;
											indirect_call_tag  = addr & 0xFFFF;
										}
										break;
									}
				default: {
									 errs() << "Unrecognized type: " << line << "\n";
									 break;
								 }
			}
		}

		errs() << "ptwrite_chunk: " << HEX(ptwrite_chunk_addr) << "\n";
		errs() << "ptwrite: " << HEX(ptwrite_addr) << "\n";
		errs() << "aRet: " << HEX(aRet_addr) << "\n";
		errs() << "indirect_call: " << HEX(indirect_call_addr) << "\n";

		if (ptwrite_chunk_addr == 0)
			llvm_unreachable("no ptwrite_chunk found\n");

		// Some sanity checks for assumptions made in optimizations
		if (ptwrite_chunk_addr > 0xFFFFFFFF)
			llvm_unreachable("ptwrite_chunk base address exceeds 32-bit, which violates an optimization assumption\n");
		if (ptwrite_tag == aRet_tag || ptwrite_tag == indirect_call_tag || aRet_tag == indirect_call_tag)
			llvm_unreachable("aRet, ptwrite, and indirect_call need to have unique lower 2 bytes\n");

		// get a sub version for address-taken functions
		for (auto F : AT_Functions) {
			string funcName = F->getName();
			uint64_t addr = AllFuncName2Addr[funcName];
			Function * func = AllAddr2Func[addr];

			AT_Func2Addr[F] = addr;
			AT_Addr2Func[addr] = func;
		}
	}

	void doAnalysis(PTA * pta)
	{
		// return flag
		uint64_t return_num = 0;
		uint64_t BB_packet_local;
		while(1) {
			BB_packet_local = getNextPacket();
#ifdef DUMP_NODE
			BB_packet = BB_packet_local;
#endif
			if (BB_packet_local <= 0x400000) {

				DR((*ofs) << "B: " << BB_packet_local << " _" << HEX(BB_packet_local +  ptwrite_chunk_addr) \
						<< " - " << global_counter << "\n");
				DR(ofs->flush());

				KB3Instruction * curKB3Instr = BBID2KB3Instrs[BB_packet_local];
				if (return_num) {
					curKB3Instr->popSomeUntil(return_num);
					return_num = 0;
				}
				curKB3Instr->execute(pta);

				DS(auto iter = BB_statistics.find(BB_packet_local));
				DS(if (iter == BB_statistics.end()))
					DS(	BB_statistics[BB_packet_local] = 1);
				DS(else)
					DS(	iter->second++);

				DS(BB_total++);

			} else if (BB_packet_local < 0xf0000000) {
				// if the packet value is larger than 0x400000 -- the default entry for binaries
				// on x64, it means this packet is a return packet:
				//
				// 1) BBID/index/condition is less than 0x400000
				// 2) indirect call/jmp target is consumed by analysis
				//return_num++;
				DR((*ofs) << "R: " << HEX(BB_packet_local) << " - " << global_counter <<  "\n");
				DR(ofs->flush());

				/*	
						if (PTA::CallStack.size() == KB3Instruction::ExecuteLaterVec.size()) {
						llvm_unreachable("SHIT");
						}
						*/

				unsigned poppedNum = PTA::popOnce();
				KB3Instruction * curKB3Instr = nullptr; 

				curKB3Instr = KB3Instruction::ExecuteLaterVec.back();
				KB3Instruction::ExecuteLaterVec.pop_back();

				if (curKB3Instr)
					curKB3Instr->execute(pta);

				DS(ret_total++);

			} else {
				// so, if return address is too large, it is highly this is a return to library 
				// function. This library function is not pushed on call stack, and we should not
				// pop a function.
			}
		}
	}
}
