#ifndef __CONTROL_FLOW_MONITOR_H__
#define __CONTROL_FLOW_MONITOR_H__

#include "common.h"
#include "Flags.h"

class PTA;

namespace PTCFI {
	DS(extern uint64_t idx_total);
	DS(extern uint64_t cnd_total);
	extern uint64_t BB_packet;

	class KB3CallInvokeInstruction;

	extern string PinTraceFileName;
	//extern uint64_t BB_packet;
	extern raw_fd_ostream * ofs;
	extern raw_fd_ostream * ofs2;
	extern uint64_t ptwrite_chunk_addr;
	extern DenseMap<uint64_t, Function *> AT_Addr2Func;
	extern DenseMap<Function *, uint64_t> AT_Func2Addr;
	extern unordered_set<Function *> SensitiveFuncs;
	extern unordered_set<KB3CallInvokeInstruction *> AllKB3CallInvokeInstrs;

	// for trace reading
	typedef struct {
		uint64_t addr;
	} RecordType;
	extern RecordType * nextPacket;
	extern RecordType * currArrayEnd;
	extern uint64_t global_counter;

	void swapBufs();
	void initializePinTrace(string PinTraceFileName);
	inline uint64_t getNextPacket() {
		if (nextPacket == currArrayEnd)
			swapBufs();
		DR(global_counter++);
		return (nextPacket++)->addr;
	}

	// main modules
	void PurifyModule2(Module * module);
	void ProcessModule(Module * module, PTA * pta);
    void HandleBBInfoFile(string BBInfoFile, Module * M);
	void doAnalysis(PTA *);
}

#endif
