#ifndef TCFS_ANDERSEN_H
#define TCFS_ANDERSEN_H

#include "common.h"

#include "NodeFactory.h"
#include "PTAAction.h"

using namespace PTCFI;

class PTA
{

public:
	NodeFactory *nodeFactory = nullptr;

	PTA(llvm::Module *M) {
		if (!NodeFactory::dataLayout) {
			NodeFactory::dataLayout = M->getDataLayout();
            NodeFactory::myContext = &M->getContext();
		}
		nodeFactory = new NodeFactory();
		nodeFactory->handleGlobals(M);
	}
	
	static uint64_t succeed_count;
	static uint64_t empty_count;
	static uint64_t fail_count;
	static const BasicBlock *CurrentBlock;
	static vector<const BasicBlock*> ShadowBlocks;
	static vector<const Function*> CallStack;

	const llvm::Value* query(NodeAction * nodeAction);
	void initializeMainArg(llvm::Module&);

	void createArgumentNodes(const Function *func);

	// call this before entering into the target function, but after
	// initialize the function's arguments
	//void pushCallStack(const Function *func);

	// call this after handling return
	//void popCallStack(const Function *func);
	
	static unsigned popOnce();
	static void dumpCallStack();
	static void dump_call_count();
	static void clean_count();
};

#endif
