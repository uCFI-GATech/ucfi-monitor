#ifndef __KB3_INSTRUCTION__
#define __KB3_INSTRUCTION__

#include "common.h"

#include "PTA.h"
#include "PTAAction.h"

namespace PTCFI {
	using namespace llvm;
	using namespace std;

    class KB3CallInvokeInstruction;
	extern uint64_t global_id;
    extern const Instruction * curIndirectCallInst;
    extern KB3CallInvokeInstruction * curKB3CallInvokeInstr;

	class KB3Instruction {
		protected:
		public:
			uint64_t addrCache = 0;
			const Function * fCache = nullptr;
			NodeAction * indirectNodeCache = nullptr;
			NodeAction * targetNodeAct = nullptr;

		public:
			const Value * calledValue = nullptr;
			const llvm::Instruction* inst = nullptr;
			NodeAction* ptaAction = nullptr;
			const llvm::Function* func = nullptr;
			KB3Instruction * next = nullptr;
			
			KB3Instruction();
			KB3Instruction(const BasicBlock * bb_);
			KB3Instruction(const llvm::Instruction* inst);
			void shouldUpdateBB();

			virtual ~KB3Instruction() {}
			//virtual const Function * checkCFI(PTA * pta, uint64_t & validTarget);
			//virtual const Function * checkCFI(PTA * pta, NodeAction * nodeAct);

		public:
			void execute(PTA* pta);
			void popSomeUntil(uint64_t);
			
			static SmallVector<KB3Instruction *, 16> ExecuteLaterVec;
	};

	class KB3CallInvokeInstruction: public KB3Instruction {
		public:
			KB3CallInvokeInstruction(llvm::CallInst* callInst_, llvm::InvokeInst* invokeInst_);
	};

	class KB3RetInstruction: public KB3Instruction {
		public:
			KB3RetInstruction(llvm::ReturnInst* retInst_) :
				KB3Instruction(retInst_) {
				}
	};

	class KB3BranchInstruction : public KB3Instruction {
		public:
			KB3BranchInstruction(llvm::BranchInst* branchInst_) :
				KB3Instruction(branchInst_) {
				}
	};
}

#endif 
