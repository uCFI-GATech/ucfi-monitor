#include "common.h"
#include "PTA.h"
#include "ControlFlowMonitor.h"

const BasicBlock* PTA::CurrentBlock = nullptr;
vector<const BasicBlock*> PTA::ShadowBlocks;
vector<const Function*> PTA::CallStack;

unordered_map<int64_t, int64_t> call_count;
uint64_t PTA::succeed_count = 0;
uint64_t PTA::empty_count = 0;
uint64_t PTA::fail_count = 0;

void PTA::clean_count()
{
	call_count.clear();
}

void PTA::dump_call_count()
{
	(*ofs) << "\nsize of call_count " << call_count.size() << "\n";
	uint64_t total_calls = 0;
	for (auto iter : call_count) {
		total_calls += iter.second;
		(*ofs) << "There are " << format("%10lu", iter.second) 
			   << " icalls having " << format("%lu", iter.first) << " targets\n";
	}
	(*ofs) << "There are totally " << total_calls << " indirect calls\n";
	auto iter = call_count.find(1);
	if (iter != call_count.end())
		total_calls -= iter->second;
	(*ofs) << total_calls << " of them have >1 targets\n";
	(*ofs) << PTA::succeed_count << " succeed\n"
		   << PTA::fail_count << " fail\n"
		   << PTA::empty_count << " empty\n";
}

unsigned PTA::popOnce()
{
	const Function *topFunc = nullptr;
	vector<NodeAction*> * nodes = nullptr;

	unsigned poppedNum = 0;

	do {
		if (PTA::CallStack.empty())
			llvm_unreachable("callstack is empty");

		topFunc = PTA::CallStack.back();
		DCS((*ofs) << "  1 pop " << topFunc->getName().str() << '\n');
		DCS(ofs->flush());

		// pop block
		PTA::CurrentBlock = PTA::ShadowBlocks.back();
		PTA::ShadowBlocks.pop_back();
		PTA::CallStack.pop_back();
		poppedNum++;

    //errs() << PTA::CurrentBlock->getParent()->getName()
    //       << " ======= "
    //       << PTA::CallStack.back()->getName()
    //       << "\n";

		if (PTA::CurrentBlock->getParent() == PTA::CallStack.back())
			break;

	} while (1);

	for (auto func : PTA::CallStack) {
		DCS((*ofs) << "callstack: "<< func->getName().str() << '\n');
		DCS(ofs->flush());
	}

	nodes = Func2Nodes[topFunc];
	for (NodeAction *na : *nodes) {
		na->decCursor();
	}

	return poppedNum;
}

const llvm::Value* PTA::query(NodeAction * nodeAction) {
	Node *targetNode = nodeAction->act(nodeFactory);

#ifdef DUMP_STATISTICS
	int64_t count =  targetNode->count;
	auto res = call_count.find(count);
	if (res == call_count.end()) {
		call_count[count] = 1;
	} else {
		res->second++;
	}
#endif

	if (!targetNode->mem || targetNode->mem->empty()) {
		DRE((*ofs) << "uCFI empty\n");
		DS(empty_count++);
		//ofs->flush();
		//llvm_unreachable("uCFI empty 1");
		return nullptr;
	} else {
		auto iter = targetNode->mem->find(targetNode->startOffset);
		if (iter == targetNode->mem->end()) {
			DRE((*ofs) << "uCFI empty\n");
			DRE(ofs->flush());
			DS(empty_count++);
			//llvm_unreachable("uCFI empty 2");
			return nullptr;
		}
		Node * node = iter->second;
		const Value *target = node->value;
		return target;
	}
}

void PTA::initializeMainArg(llvm::Module &M) {
	for (const Function &f: M){
		//if (f.getName().equals("main") || 
				//f.getName().startswith("_GLOBAL__sub_I_") ||
				//f.getName().startswith("__cxx_global")) {
		if (f.getName().equals("main")) {
			// push 'main'
			CallStack.push_back(&f);
			ShadowBlocks.push_back(nullptr);
      PTA::CurrentBlock = &f.getEntryBlock();
			DCS((*ofs) << "push " << f.getName() << '\n');

			Function::const_arg_iterator fItr = f.arg_begin();

			while (fItr != f.arg_end()) {
				const Argument* formal = fItr;
				if (!formal->getType()->isPointerTy()) {
					++fItr;
					continue;
				}
				
				NodeAction *valCreateStackNode = StackNode::getStackNode(formal, &f);
				valCreateStackNode->incCursor();

				Node *valNode = valCreateStackNode->act(nodeFactory);

				valNode->mem = new NodeMapType ();
				(*(valNode->mem))[0] = new Node(Node::UNDEFINED, nullptr);

				++fItr;
			}
			
			vector<NodeAction*> * nodes = Func2Nodes[&f];
			if (nodes == nullptr) {
				nodes = new vector<NodeAction *>;
				Func2Nodes[&f] = nodes;
			}
			for (NodeAction *na : *nodes) {
				na->incCursor();
			}
		}
	}
}

/*
void PTA::pushCallStack(const Function* func) {
	ShadowBlocks.push_back(CurrentBlock);
	
	if (!func)
		llvm_unreachable("push null function");
	
	CallStack.push_back(func);

	vector<NodeAction*> &nodes = Func2Nodes[func];
	for (NodeAction *na : nodes) {
		if (!na->isArgument) {
			na->incCursor();
		}
	}

}

void PTA::popCallStack(const Function *func) {
	// pop block
	PTA::CurrentBlock = ShadowBlocks.back();
	ShadowBlocks.pop_back();
	
	// pop called function and restore nodes
	const Function *topFunc = CallStack.back();
	if (!topFunc)
		llvm_unreachable("the top function is null");
	vector<NodeAction*> &nodes = Func2Nodes[topFunc];
	for (NodeAction *na : nodes) {
		na->decCursor();
	}
	CallStack.pop_back();

}
*/

	void PTA::createArgumentNodes(const Function *func) {
		Function::const_arg_iterator fItr = func->arg_begin();
		while (fItr != func->arg_end()) {
			const Argument* formal = fItr;

			StackNode::getStackNode(formal, func);

			++fItr;
		}

		if (func->getFunctionType()->isVarArg()) {
			StackNode::getStackNode(func, func);
		}
	}

 void PTA::dumpCallStack() {
	DCS((*ofs) << "call stack: ");
	for (const Function *func : CallStack) {
		if (func)
			DCS((*ofs) << func->getName() << ' ');
		else
			DCS((*ofs) << "null ");
	}
	DCS((*ofs) << '\n');
 }
