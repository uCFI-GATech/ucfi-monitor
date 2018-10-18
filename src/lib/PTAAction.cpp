#include "common.h"
#include "PTA.h"
#include "Flags.h"
#include "PTAAction.h"
#include "ControlFlowMonitor.h"

namespace PTCFI {
	DenseMap<const Function*, vector<NodeAction*> *> Func2Nodes;

	Node* NodeAction::act(NodeFactory *nodeFactory) {}

	void NodeAction::popSomeUntil(uint64_t return_num) 
	{
		DCS((*ofs) << "pop once until " << parentF->getName().str() << '\n');
		DCS(ofs->flush());

		const Function *topFunc = nullptr;
		vector<NodeAction*> * nodes = nullptr;

		for (; return_num != 0; return_num--) {
			if (PTA::CallStack.empty())
				llvm_unreachable("callstack is empty");

			topFunc = PTA::CallStack.back();
			DCS((*ofs) << "  1 pop " << topFunc->getName().str() << '\n');
			DCS(ofs->flush());

			// pop block
			PTA::CurrentBlock = PTA::ShadowBlocks.back();
			PTA::ShadowBlocks.pop_back();
			PTA::CallStack.pop_back();

			nodes = Func2Nodes[topFunc];
			for (NodeAction *na : *nodes) {
				na->decCursor();
			}
		}
		DCS(PTA::dumpCallStack());

		if (isInCallback) return;

		while (true) {
			if (PTA::CallStack.empty())
				llvm_unreachable("main retuns");

			topFunc = PTA::CallStack.back();
			if (topFunc == parentF) break;
			if (!topFunc) llvm_unreachable("the top function is null");

			DCS((*ofs) << "  2 pop " << topFunc->getName().str() << '\n');
			DCS(ofs->flush());

			// pop block
			PTA::CallStack.pop_back();
			PTA::CurrentBlock = PTA::ShadowBlocks.back();
			PTA::ShadowBlocks.pop_back();

			nodes = Func2Nodes[topFunc];
			for (NodeAction *na : *nodes)
				na->decCursor();
		}
	}

	FunctionNode::FunctionNode(const Function * func) {
		node = NodeFactory::GlobalValueNodes[func];
	}

	Node * FunctionNode::act(NodeFactory *nodeFactory) {
		return node;
	}

	BasicBlockUpdateNode::BasicBlockUpdateNode(const BasicBlock *bb_) {
		bb = bb_;
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}
	}

	Node* BasicBlockUpdateNode::act(NodeFactory *nodeFactory) {
		if (shouldUpdateBB) {
			PTA::CurrentBlock = bb;
		}

		return nullptr;
	}

	// UndefineNode

	UndefinedNode * UndefinedNode::instance = nullptr;
	UndefinedNode * UndefinedNode::getUndefinedNode(const Value *val_) {
		if (!instance) {
			instance = new UndefinedNode(val_);
		}
		return instance;
	}

	Node* UndefinedNode::act(NodeFactory *nodeFactory) {
		return NodeFactory::GLOBAL_UNDEFINED_NODE;
	}

	// NullNode

	NullNode * NullNode::instance = nullptr;
	NullNode * NullNode::getNullNode(const Value *val_) {
		if (!instance) {
			instance = new NullNode(val_);
		}
		return instance;
	}

	Node* NullNode::act(NodeFactory *nodeFactory) {
		return NodeFactory::GLOBAL_NULL_NODE;
	}

	// StackNode

	DenseMap<const Value*, NodeAction*> StackNode::StackNodeMap;

	NodeAction * StackNode::getStackNode(const Value *val_, const Function *func_) {
		if (isa<UndefValue>(val_) || isa<ConstantInt>(val_) || 
				isa<ConstantFP>(val_) || isa<ConstantExpr>(val_))
			return UndefinedNode::getUndefinedNode(val_);

		if (isa<ConstantArray>(val_) || isa<ConstantStruct>(val_) || isa<ConstantVector>(val_)) {
			errs() << "WARN: constant array/struct/vector\n";
			return UndefinedNode::getUndefinedNode(val_);
		}

		if (isa<ConstantPointerNull>(val_))
			return NullNode::getNullNode(val_);

		if (StackNodeMap.find(val_) != StackNodeMap.end()) {
			return StackNodeMap[val_];
		} else {
			if (const BitCastInst *bcInst = dyn_cast<BitCastInst>(val_)) {
				NodeAction* generalNode = 
					GeneralNode::getGeneralNode(bcInst->getOperand(0), func_);
				if (!generalNode)
					llvm_unreachable("bitcast src node is null");
				StackNodeMap[val_] = generalNode;
				return generalNode;
			} else {
				StackNode *newNode = new StackNode(val_, func_);
				StackNodeMap[val_] = newNode;
				return newNode;
			}
		}
	}

	Node* StackNode::act(NodeFactory *nodeFactory) {
		if (isUndefinedVal || isNullVal) {
			return NodeFactory::GLOBAL_UNDEFINED_NODE;
		}

		if ((unsigned)cursor < nodeSize) {
			return nodes[cursor];
		} else if (cursor == -1) {
			DCS((*ofs) << *val <<  ' ' << cursor << '\n');
			DCS((*ofs) << "WARN: out of bound\n");
			const Function *targetFunc = parentF;
			PTA::ShadowBlocks.push_back(PTA::CurrentBlock);
			PTA::CallStack.push_back(targetFunc);
			DCS((*ofs) << "push " << targetFunc->getName() << '\n');
			vector<NodeAction*> * targetNodes = Func2Nodes[targetFunc];
			for (NodeAction *na : *targetNodes) {
				na->incCursor();
			}
			return nodes[cursor];
		} else {
			llvm_unreachable("out of bound");
		}
	}

	StackNode::StackNode(const Value *val_, const Function *func_) : NodeAction(val_) {
		parentF = func_;
		if (isa<ConstantPointerNull>(val))
			isNullVal = true;

		if (isa<UndefValue>(val))
			isUndefinedVal = true;

		if (isNullVal || isUndefinedVal)
			llvm_unreachable("stack node is null/undefined");

		if (isa<AllocaInst>(val))
			type = Node::SPECIAL_STACK;
		else
			type = Node::STACK;

		// save 'this' if it does not exist	
		vector<NodeAction*>	* nodeActions = Func2Nodes[func_];
		if (nodeActions == nullptr) {
			nodeActions = new vector<NodeAction *>;
			Func2Nodes[func_] = nodeActions;
		}

		bool found = false;
		for (NodeAction *na : *nodeActions) {
			if (na->val == val) {
				found = true;
				break;
			}
		}

		if (!found) {
			nodeActions->push_back(this);
		}

		// initialize 'nodes'
		nodes = (Node**) calloc(NODE_INIT_SIZE, sizeof(Node*));
		for (unsigned i = 0; i < NODE_INIT_SIZE; ++i) {
			nodes[i] = new Node(type, val_);
		}
		nodeSize = NODE_INIT_SIZE;
	}

	// HeapNode

	DenseMap<const Value*, HeapNode*> HeapNode::HeapNodeMap;

	HeapNode::HeapNode(const Value *val_, const Function *func_) : NodeAction(val_) {
		parentF = func_;
		if (isa<ConstantPointerNull>(val))
			isNullVal = true;

		if (isa<UndefValue>(val))
			isUndefinedVal = true;

		if (isNullVal || isUndefinedVal)
			llvm_unreachable("heap node is null/undefined");

		// save 'this' if it does not exist	
		vector<NodeAction*>	* nodeActions = nullptr;
		auto iter = Func2Nodes.find(func_);
		if (iter == Func2Nodes.end()) {
			nodeActions = new vector<NodeAction *>;
			Func2Nodes[func_] = nodeActions;
		} else
			nodeActions = Func2Nodes[func_];

		bool found = false;
		for (NodeAction *na : *nodeActions) {
			if (na->val == val) {
				found = true;
				break;
			}
		}

		type = Node::HEAP;

		if (!found) {
			//errs() << "add heap node: " << func_->getName() << " " << *val << '\n';
			nodeActions->push_back(this);
		}

		// initialize 'nodes'
		nodes = (Node**) calloc(NODE_INIT_SIZE, sizeof(Node*));
		for (unsigned i = 0; i < NODE_INIT_SIZE; ++i) {
			nodes[i] = new Node(type, val_);
		}
		nodeSize = NODE_INIT_SIZE;
	}

	HeapNode * HeapNode::getHeapNode(const Value *val_, const Function *func_) {
		if (isa<UndefValue>(val_) || isa<ConstantInt>(val_) || 
				isa<ConstantPointerNull>(val_) || isa<ConstantFP>(val_))
			llvm_unreachable("getHeapNode Undef/Null/ConstantInt");
		if (isa<ConstantArray>(val_) || isa<ConstantStruct>(val_) || isa<ConstantVector>(val_)) {
			llvm_unreachable("WARN: constant array/struct/vector");
		}

		if (HeapNodeMap.find(val_) != HeapNodeMap.end()) {
			return HeapNodeMap[val_];
		} else {
			HeapNode *newNode = new HeapNode(val_, func_);
			HeapNodeMap[val_] = newNode;
			return newNode;
		}
	}

	HeapNode * HeapNode::forceCreateHeapNode(const Value *val, const Function *func) {
		// update 'nodes'
		if (cursor == -1) {
			DCS((*ofs) << "WARN: out of bound\n");
			const Function *targetFunc = parentF;
			PTA::ShadowBlocks.push_back(PTA::CurrentBlock);
			PTA::CallStack.push_back(targetFunc);
			DCS((*ofs) << "push " << targetFunc->getName() << '\n');
			vector<NodeAction*> * targetNodes = Func2Nodes[targetFunc];
			for (NodeAction *na : *targetNodes) {
				na->incCursor();
			}
		}
		Node * newNode = new Node(Node::HEAP, val);
		nodes[cursor] = newNode;
		return this;
	}

	Node* HeapNode::act(NodeFactory *nodeFactory) {
		if (isUndefinedVal) {
			return NodeFactory::GLOBAL_UNDEFINED_NODE;
			//return new Node(Node::UNDEFINED, nullptr);
		}
		if (isNullVal) 
			return NodeFactory::GLOBAL_NULL_NODE;

		if (cursor >= 0 && cursor < nodeSize) {
			return nodes[cursor];
		} else if (cursor == -1) {
			DCS((*ofs) << "WARN: out of bound\n");
			const Function *targetFunc = parentF;
			PTA::ShadowBlocks.push_back(PTA::CurrentBlock);
			PTA::CallStack.push_back(targetFunc);
			DCS((*ofs) << "push " << targetFunc->getName() << '\n');
			vector<NodeAction*> * targetNodes = Func2Nodes[targetFunc];
			for (NodeAction *na : *targetNodes) {
				na->incCursor();
			}
			return nodes[cursor];
		} else {
			llvm_unreachable("out of bound");
			//return NodeFactory::GLOBAL_UNDEFINED_NODE;
			return new Node(Node::UNDEFINED, nullptr);
		}
	}

	void NodeAction::incCursor() { 
		cursor += 1;
		if (cursor == nodeSize) {
			nodeSize += INC_SIZE;
			nodes = (Node**) realloc(nodes, sizeof(Node*) * nodeSize);
			for (unsigned i = 0; i < INC_SIZE; ++i) {
				nodes[cursor+i] = new Node(type, val);
			}
		}
	}

	void NodeAction::decCursor() {
		if (cursor >= 0) {
			if (cursor >= nodeSize) {
				errs() << *val << '\n';
				llvm_unreachable("out of bound");
			}

			if (nodes[cursor]->type == Node::SPECIAL_STACK) {
				nodes[cursor]->mem->clear();
			}

			cursor -= 1;
		}
	}

	// GlobalNode

	DenseMap<const Value*, GlobalNode*> GlobalNode::GlobalNodeMap;

	GlobalNode * GlobalNode::getGlobalNode(const Value *val_) {
		if (GlobalNodeMap.find(val_) != GlobalNodeMap.end()) {
			return GlobalNodeMap[val_];
		} else {
			GlobalNode *newNode = new GlobalNode(val_);
			GlobalNodeMap[val_] = newNode;
			return newNode;
		}
	}

	Node* GlobalNode::act(NodeFactory *nodeFactory) {
		return node;
	}

	GlobalNode::GlobalNode(const Value *val_) : NodeAction(val_) {
		const GlobalValue *gVal = dyn_cast<GlobalValue>(val);
		if (!gVal) 
			llvm_unreachable("not global variable");
		DenseMap<const Value*, Node*>::iterator gIt = NodeFactory::GlobalValueNodes.find(val);
		if (gIt == NodeFactory::GlobalValueNodes.end())
			llvm_unreachable("cannot find global variable");

		node = gIt->second;
	}

	// GeneralNode

	// 'func' is null, only for PTAA query
	NodeAction * GeneralNode::getGeneralNode(const Value *val, const Function* func) {
		if (isa<UndefValue>(val) || isa<ConstantInt>(val) || isa<ConstantFP>(val) || isa<ConstantExpr>(val))
			return UndefinedNode::getUndefinedNode(val);

		if (isa<ConstantPointerNull>(val))
			return NullNode::getNullNode(val);

		bool unused;
		// global?
		if (const GlobalValue *gVal = dyn_cast<GlobalValue>(val)) {
			return GlobalNode::getGlobalNode(val);
		} else if (NodeFactory::isHeapValue(val, nullptr, unused)) {
			if (func) {
				return HeapNode::getHeapNode(val, func);
			} else {
				NodeAction *retNode = HeapNode::HeapNodeMap[val];
				return retNode;
			}
		} else {
			if (func) {
				return StackNode::getStackNode(val, func);
			} else {
				NodeAction *retNode = StackNode::StackNodeMap[val];
				return retNode;
			}
		}
	}

	// InitArgsNode

	InitArgsNode::~InitArgsNode() {
		for (unsigned i = 0; i < actualArgNodesNum; ++i) 
			delete actualArgNodes[i];

		unsigned n = formalArgNodes.size();
		for (unsigned i = 0; i < n; ++i)
			delete formalArgNodes[i];

		if (varFuncNode)
			delete varFuncNode;

		n = varArgNodes.size();
		for (unsigned i = 0; i < n; ++i)
			delete varArgNodes[i];
	}

	InitArgsNode::InitArgsNode (ImmutableCallSite cs, const Function *target_,
			const Function *caller_) : target (target_), caller (caller_) {

		Function::const_arg_iterator fItr = target->arg_begin();
		ImmutableCallSite::arg_iterator aItr = cs.arg_begin();

		while (fItr != target->arg_end() && aItr != cs.arg_end()) {
			const Argument* formal = fItr;
			const Value* actual = *aItr;

			if (!isa<UndefValue>(actual))
				allUndefinedArgs = false;

			if (!formal->hasNUses(0)) {
				actualArgNodes.push_back(GeneralNode::getGeneralNode(actual, caller));
				formalArgNodes.push_back(StackNode::getStackNode(formal, target));
			}

			++fItr, ++aItr;
		}

		actualArgNodesNum = actualArgNodes.size();

		if (target->getFunctionType()->isVarArg()) {
			//varFuncNode = new StackNode(f, target);
			varFuncNode = StackNode::getStackNode(target, target);

			while (aItr != cs.arg_end()) {
				const Value *actual = *aItr;
				varArgNodes.push_back(GeneralNode::getGeneralNode(actual, caller));
				++aItr;
			}
		}

	}

	Node* InitArgsNode::act(NodeFactory *nodeFactory, Node **srcNodes, Node **varSrcNodes) {

		Node *retNode = nullptr;

		if (!allUndefinedArgs) {
			DN((*ofs) << "================== InitArgs Node ===============\n");
			for (unsigned i = 0; i < actualArgNodesNum; ++i) {
				Node *srcNode = srcNodes[i];
				Node *dstNode = formalArgNodes[i]->act(nodeFactory);

				DN((*ofs) << "++++++++++++++++++++++++++++++++++\n");
				DN(srcNode->dump());
				DC(srcNode->dump());
				srcNode->copy(dstNode);
				DN((*ofs) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
				DN(dstNode->dump());
				DC(dstNode->dump());
			}
			DN((*ofs) << "----------------------------------\n");

			// TODO: currently, only copy the first vararg
			if (varFuncNode && varArgNodes.size() > 0) {
				Node *srcNode = varSrcNodes[0];
				Node *dstNode = varFuncNode->act(nodeFactory);
				srcNode->copy(dstNode);
			}
		}

		return retNode;
	}

	// ExternalCallNode

	ExternalCallNode::ExternalCallNode(ImmutableCallSite cs, const Function* f) {
		bb = cs.getInstruction()->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		actionType = EXT_NOOP;
		if (ExternalLibrary::lookupName(ExternalLibrary::noopFuncs, f->getName().data())) {
			actionType = EXT_NOOP;
		} else {
			bool isReallocLike = ExternalLibrary::lookupName(ExternalLibrary::reallocFuncs, f->getName().data());
			inst = cs.getInstruction();

			if (ExternalLibrary::lookupName(ExternalLibrary::mallocFuncs, f->getName().data()) || 
					isReallocLike) {
				actionType = EXT_MALLOC;
				// create a heap node
				createHeapNode = HeapNode::getHeapNode(inst, parentF);
				//createHeapNode = new CreateHeapNode(inst);

			} else if (ExternalLibrary::lookupName(ExternalLibrary::retArg0Funcs, f->getName().data())) {
				Type* argType = cs.getArgument(0)->getType();
				actionType = EXT_RETARG0;

				// copy arg0's to ret
				//createStackNode = new CreateStackNode(inst);
				//getArg0Node = new GetGeneralNode(cs.getArgument(0));
				createStackNode = StackNode::getStackNode(inst, parentF);
				getArg0Node = GeneralNode::getGeneralNode(cs.getArgument(0), parentF);

			} else if (ExternalLibrary::lookupName(ExternalLibrary::retArg1Funcs, f->getName().data())) {
				Type* argType = cs.getArgument(1)->getType();
				actionType = EXT_RETARG1;

				// copy arg1's to ret
				//createStackNode = new CreateStackNode(inst);
				//getArg1Node = new GetGeneralNode(cs.getArgument(1));
				createStackNode = StackNode::getStackNode(inst, parentF);
				getArg1Node = GeneralNode::getGeneralNode(cs.getArgument(1), parentF);
			} else if (ExternalLibrary::lookupName(ExternalLibrary::retArg2Funcs, f->getName().data())) {
				Type* argType = cs.getArgument(2)->getType();
				actionType = EXT_RETARG2;

				// copy arg2's to ret
				//createStackNode = new CreateStackNode(inst);
				//getArg2Node = new GetGeneralNode(cs.getArgument(2));
				createStackNode = StackNode::getStackNode(inst, parentF);
				getArg2Node = GeneralNode::getGeneralNode(cs.getArgument(2), parentF);
			} else if (ExternalLibrary::lookupName(ExternalLibrary::memcpyFuncs, f->getName().data())) {
				if (!isa<UndefValue>(cs.getArgument(0)) && !isa<UndefValue>(cs.getArgument(1))) {
					actionType = EXT_MEMCPY;

					const Value * lenValue = cs.getArgument(2);
					if (const ConstantInt * CI = dyn_cast<ConstantInt>(lenValue))
						copySize = CI->getSExtValue();
					else
						copySize = 4096;
					// copy arg1's to ret and arg0
					//createStackNode = new CreateStackNode(inst);
					//getArg0Node = new GetGeneralNode(cs.getArgument(0));
					//getArg1Node = new GetGeneralNode(cs.getArgument(1));
					createStackNode = StackNode::getStackNode(inst, parentF);
					getArg0Node = GeneralNode::getGeneralNode(cs.getArgument(0), parentF);
					getArg1Node = GeneralNode::getGeneralNode(cs.getArgument(1), parentF);
				}
			} else if (ExternalLibrary::lookupName(ExternalLibrary::convertFuncs, f->getName().data())) {
				actionType = EXT_CONVERT;
			} else if (f->getName() == "llvm.va_start") {
				//const Instruction* inst = cs.getInstruction();
				//const Function* parentF = inst->getParent()->getParent();
				actionType = EXT_VASTART;

				//getArg0Node = new GetGeneralNode(cs.getArgument(0));
				//getVarArgNode = new GetStackNode(parentF);
				getArg0Node = GeneralNode::getGeneralNode(cs.getArgument(0), parentF);
				getVarArgNode = StackNode::getStackNode(parentF, parentF);
			} else {
				actionType = EXT_UNKNOWN;
				//createStackNode = new CreateStackNode(inst);
				createStackNode = StackNode::getStackNode(inst, parentF);

				for (ImmutableCallSite::arg_iterator itr = cs.arg_begin(), ite = cs.arg_end();
						itr != ite; ++itr) {
					Value* argVal = *itr;
					//getArgNodes.push_back(new GetGeneralNode(argVal));
					getArgNodes.push_back(GeneralNode::getGeneralNode(argVal, parentF));
				}
			}
		}
	}

	ExternalCallNode::~ExternalCallNode() {
		if (createHeapNode)
			delete createHeapNode;
		if (createStackNode)
			delete createStackNode;
		if (getArg0Node)
			delete getArg0Node;
		if (getArg1Node)
			delete getArg1Node;
		if (getArg2Node)
			delete getArg2Node;

		unsigned argNum = getArgNodes.size();
		for (unsigned i = 0; i < argNum; ++i)
			delete getArgNodes[i];
	}

	Node* ExternalCallNode::act(NodeFactory *nodeFactory) {

		switch(actionType) {
			case EXT_NOOP:
				break;
			case EXT_MALLOC:
				{
					if (createHeapNode) {
						createHeapNode->forceCreateHeapNode(inst, parentF);
						Node *newHeapNode = createHeapNode->act(nodeFactory);
						DN((*ofs) << "calling here\n");
						DN(newHeapNode->dump());
						DC(newHeapNode->dump());
					}
					break;
				}
			case EXT_RETARG0:
				{
					Node *srcNode = getArg0Node->act(nodeFactory);
					Node *dstNode = createStackNode->act(nodeFactory);
					srcNode->copy(dstNode);
					break;
				}
			case EXT_RETARG1: 
				{
					Node *srcNode = getArg1Node->act(nodeFactory);
					Node *dstNode = createStackNode->act(nodeFactory);
					srcNode->copy(dstNode);
					break;
				}
			case EXT_RETARG2:
				{
					Node *srcNode = getArg2Node->act(nodeFactory);
					Node *dstNode = createStackNode->act(nodeFactory);
					srcNode->copy(dstNode);
					break;
				}
			case EXT_MEMCPY:
				{
					// for memory copy, the correct way is to copy the mem content 
					// from one node to another
					Node *srcNode = getArg1Node->act(nodeFactory);
					Node *dstNode1 = getArg0Node->act(nodeFactory);
					//Node *dstNode2 = createStackNode->act(nodeFactory);
					DN(srcNode->dump());
					srcNode->deepCopy(dstNode1, copySize);
					DN(dstNode1->dump());
					//srcNode->deepCopy(dstNode2, copySize);
					break;
				}
			case EXT_VASTART:
				{
					Node *srcNode = getVarArgNode->act(nodeFactory);
					Node *dstNode = getArg0Node->act(nodeFactory);
					srcNode->deepCopy(dstNode, 8);
					break;
				}
			default:
				{
					if (createStackNode) {
						Node *dstNode = createStackNode->act(nodeFactory);
						dstNode->mem = new NodeMapType ();
						//Node *srcNode = getArgNodes[0]->act(nodeFactory);
						// TODO: currently, only copy the first argument
						//srcNode->copy(dstNode);
					}
					break;
				}
		}
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	// AllocateNode

	AllocateNode::AllocateNode (const AllocaInst *allocaInst_) : allocaInst (allocaInst_) {
		bb = allocaInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}
		//createStackNode = new CreateStackNode(allocaInst);
		createStackNode = StackNode::getStackNode(allocaInst, parentF);
	}

	Node* AllocateNode::act(NodeFactory *nodeFactory) {
		Node *newNode = createStackNode->act(nodeFactory);
		//newNode->mem = new DenseMap<int64_t, Node*> ();
		DN((*ofs) << "================ Alloca Node ================\n");
		DN((*ofs) << *allocaInst << '\n');
		DN(newNode->dump());

		//errs() << "Allocate update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	// IndirectCallNode

	IndirectCallNode::IndirectCallNode(const Instruction *inst_): inst (inst_) {
		bb = inst->getParent();
		parentF = bb->getParent();
		shouldUpdateBB = true;
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		nodes = Func2Nodes[func];
	}

	IndirectCallNode::IndirectCallNode(const Instruction *inst_, const Function *func_): inst (inst_), func (func_) {
		bb = inst->getParent();
		parentF = bb->getParent();
		shouldUpdateBB = true;
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		if (SensitiveFuncs.find(const_cast<Function*>(func)) != SensitiveFuncs.end()) {
			isTargetSensive = true;
		}

		ImmutableCallSite cs(inst);
		if (func->isDeclaration() || func->isIntrinsic()) {
			externalCallNode = new ExternalCallNode(cs, func);
		} else {
			if (!func->getReturnType()->isVoidTy()) {
				bool returnUndefined = false;
				if (NodeFactory::isHeapValue(inst, func, returnUndefined)) {
					retHeapNode = HeapNode::getHeapNode(cs.getInstruction(), 
							parentF);
					//if (retHeapNode->cursor == -1) {
					retHeapNode->incCursor();
					//}
				} else if (!returnUndefined) {
					retStackNode = StackNode::getStackNode(cs.getInstruction(), parentF);
					//if (retStackNode->cursor == -1) {
					retStackNode->incCursor();
					//}
				}
			}
			initArgsNode = new InitArgsNode(cs, func, parentF);
			unsigned n = initArgsNode->actualArgNodesNum;
			srcNodes = (Node**) calloc(n, sizeof(Node*));
			argSrcNodes = (Node**) calloc(1, sizeof(Node*));
		}

		nodes = Func2Nodes[func];
	}

	NodeAction* IndirectCallNode::getIndirectNode(const Function *func) {
		NodeAction *retNode = nullptr;
		DenseMap<const Function*, IndirectCallNode*>::iterator it = ActivatedTargets.find(func);
		if (it == ActivatedTargets.end()) {
			IndirectCallNode *newNode = new IndirectCallNode(inst, func);
			ActivatedTargets[func] = newNode;
			retNode = newNode;
		} else {
			retNode = it->second;
		}

		return retNode;
	}

	Node* IndirectCallNode::act(NodeFactory *nodeFactory) {

		Node * retValue = nullptr;

		if (externalCallNode) {
			externalCallNode->act(nodeFactory);
		} else if (isTargetSensive) {
			PTA::ShadowBlocks.push_back(bb);
			PTA::CallStack.push_back(func);
			retValue = (Node *) 1;
			DCS((*ofs) << "indirect push " << func->getName().str() << '\n');
			DCS(ofs->flush());

			if (retStackNode) {
				Node *returnValNode = retStackNode->act(nodeFactory);
				nodeFactory->returnValStack.push_back(returnValNode);
			} else if (retHeapNode) {
				Node *returnValNode = retHeapNode->act(nodeFactory);
			}

			// save the parameter nodes
			if (!initArgsNode->allUndefinedArgs) {
				unsigned n = initArgsNode->actualArgNodesNum;
				for (unsigned i = 0; i < n; ++i)
					srcNodes[i] = initArgsNode->actualArgNodes[i]->act(nodeFactory);

				if (initArgsNode->varFuncNode && initArgsNode->varArgNodes.size() > 0)
					argSrcNodes[0] = initArgsNode->varArgNodes[0]->act(nodeFactory);
			}	

			// inc the cursor of target function
			for (NodeAction *na : *nodes)
				na->incCursor();

			// copy parameters to arguments	
			if (!initArgsNode->allUndefinedArgs)
				initArgsNode->act(nodeFactory, srcNodes, argSrcNodes);
		}

		//errs() << "IndirectCall update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return retValue;
	}

	IndirectCallNode::~IndirectCallNode() {
		if (externalCallNode)
			delete externalCallNode;
		if (initArgsNode)
			delete initArgsNode;
		if (retStackNode)
			delete retStackNode;
		if (retHeapNode)
			delete retHeapNode;
	}

	// CallInvokeNode
	CallInvokeNode::CallInvokeNode (ImmutableCallSite cs) {
		inst = cs.getInstruction();
		bb = cs.getInstruction()->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		targetFunc = cs.getCalledFunction();
		nodes = Func2Nodes[targetFunc];
		if (nodes == nullptr) {
			nodes = new vector<NodeAction *>;
			Func2Nodes[targetFunc] = nodes;
		}
		if (!targetFunc)
			llvm_unreachable("cannot handle indirect call here");

		if (SensitiveFuncs.find(const_cast<Function*>(targetFunc)) != SensitiveFuncs.end()) {
			isTargetSensive = true;
		}

		if (targetFunc->isDeclaration() || targetFunc->isIntrinsic()) {
			externalCallNode = new ExternalCallNode(cs, targetFunc);
		} else {
			if (!targetFunc->getReturnType()->isVoidTy()) {
				bool returnUndefined = false;
				if (NodeFactory::isHeapValue(inst, targetFunc, returnUndefined)) {
					retHeapNode = HeapNode::getHeapNode(cs.getInstruction(), parentF);
				} else if (!returnUndefined){
					retStackNode = StackNode::getStackNode(cs.getInstruction(), parentF);
				}
			}
			initArgsNode = new InitArgsNode(cs, targetFunc, parentF);
			unsigned n = initArgsNode->actualArgNodesNum;
			srcNodes = (Node**) calloc(n, sizeof(Node*));
			argSrcNodes = (Node**) calloc(1, sizeof(Node*));
		}
	}

	Node* CallInvokeNode::act(NodeFactory *nodeFactory) {
		Node *retNode = nullptr;

		/* push start */
		if (isTargetSensive) {	
			PTA::ShadowBlocks.push_back(bb);
			PTA::CallStack.push_back(targetFunc);
			retNode = (Node *) 1;
			DCS((*ofs) << "push " << targetFunc->getName().str() << '\n');
			DCS(ofs->flush());
		}
		/* push end */

		DN((*ofs) << "===================== Call Node =============\n");
		DN((*ofs) << *inst << "\n");

		if (externalCallNode) {
			retNode = externalCallNode->act(nodeFactory);
		} else if (isTargetSensive) {
			if (retStackNode) {
				Node *returnValNode = retStackNode->act(nodeFactory);
				DCS((*ofs) << "stack push return node: " << returnValNode << '\n');
				DCS(ofs->flush());
				nodeFactory->returnValStack.push_back(returnValNode);
			} else if (retHeapNode) {
				DCS((*ofs) << "heap return node \n");
				DCS(ofs->flush());
				retHeapNode->forceCreateHeapNode(inst, parentF);
				Node *returnValNode = retHeapNode->act(nodeFactory);
				DN(returnValNode->dump());
			}

			// save the parameter nodes before increase the cursor
			// to correctly handle calling to itself
			if (!initArgsNode->allUndefinedArgs) {
				unsigned n = initArgsNode->actualArgNodesNum;
				for (unsigned i = 0; i < n; ++i) 
					srcNodes[i] = initArgsNode->actualArgNodes[i]->act(nodeFactory);

				if (initArgsNode->varFuncNode && initArgsNode->varArgNodes.size() > 0)
					argSrcNodes[0] = initArgsNode->varArgNodes[0]->act(nodeFactory);
			}	

			// inc the cursor of target function
			for (NodeAction *na : *nodes) 
				na->incCursor();

			// copy parameters to arguments	
			if (!initArgsNode->allUndefinedArgs)
				initArgsNode->act(nodeFactory, srcNodes, argSrcNodes);
		}

		//errs() << "CallInvoke update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return retNode;
	}

	Node* RetNode::act(NodeFactory *nodeFactory) {
		if (getRetNode) {

			// get the saved return value on stack
			Node *savedRetNode = nodeFactory->returnValStack.back();
			DCS((*ofs) << "pop return node: " << savedRetNode << '\n');
			DCS(ofs->flush());
			nodeFactory->returnValStack.pop_back();
			Node *retValNode = getRetNode->act(nodeFactory);

			DN((*ofs) << "================ RetNode ==============\n");
			DN((*ofs) << *retInst << "\n");
			DN(retValNode->dump());
			DC(retValNode->dump());
			retValNode->copy(savedRetNode);
			DN((*ofs) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
			DN(savedRetNode->dump());
			DC(savedRetNode->dump());
			DN((*ofs) << "---------------------------------------\n");

		}

		//errs() << "Ret update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	Node* LoadNode::act(NodeFactory *nodeFactory) {
		Node *srcNode = getGeneralNode->act(nodeFactory);
		Node *dstNode = createStackNode->act(nodeFactory);

		DN((*ofs) << "================= LoadNode ===========\n");
		DN((*ofs) << *loadInst << "\n");
		DN(srcNode->dump());
		DN((*ofs) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");

#ifdef DUMP_STATISTICS
		Node * realSrcNode = srcNode->getNode();
		realSrcNode->copy(dstNode);

		uint64_t tmp;
		if (__builtin_umull_overflow(dstNode->count, srcNode->count, &tmp))
			dstNode->count = ULONG_MAX;
		else
			dstNode->count *= srcNode->count;
		//if (dstNode->count != 1)
		//(*ofs) << "  LoadNode:\n"
		//	   << "  srcNode->count: " << srcNode->count << "\n"
		//	   << "  realSrcNode->count: " << realSrcNode->count << "\n"
		//	   << "  dstNode->count: " << dstNode->count << "\n";
#else
		srcNode->getNode()->copy(dstNode);
#endif
		DN(dstNode->dump());
		DC(dstNode->dump());
		DN((*ofs) << "---------------------------------------\n");

		//errs() << "Load update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	StoreNode::StoreNode (const StoreInst *storeInst_) : storeInst (storeInst_) {
		bb = storeInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		//srcGetGeneralNode = new GetGeneralNode(storeInst->getValueOperand());
		//dstGetGeneralNode = new GetGeneralNode(storeInst->getPointerOperand());
		srcGetGeneralNode = GeneralNode::getGeneralNode(storeInst->getValueOperand(), parentF);
		dstGetGeneralNode = GeneralNode::getGeneralNode(storeInst->getPointerOperand(), parentF);
	}

	Node* StoreNode::act(NodeFactory *nodeFactory) {
		Node *srcNode = srcGetGeneralNode->act(nodeFactory);
		Node *dstNode = dstGetGeneralNode->act(nodeFactory);

		if (!srcNode || !dstNode) 
			llvm_unreachable("srcNode/dstNode is null");

		DN((*ofs) << "================= StoreNode =============\n");
		DN((*ofs) << *storeInst << "\n");
		DN(srcNode->dump());
		DN(dstNode->dump());
		DN((*ofs) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");

		Node * tmpNode = dstNode->getNode();
		if (dstNode->mem == NodeFactory::GLOBAL_UNDEFINED_NODE->mem) {
			DRE((*ofs) << "update to undef node @" << dstNode->mem << "\n");
			//dstNode->dump();
		}

		srcNode->copy(tmpNode);

		DN(dstNode->dump());
		DC(dstNode->dump());
		DN((*ofs) << "-----------------------------\n");

		//errs() << "Store update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	GetElementPtrNode::GetElementPtrNode (const GetElementPtrInst *gepInst_) 
		: gepInst (gepInst_) 
	{
		bb = gepInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		for (unsigned i = 0; i < 8; ++i) {
			cachedElementOffset[i] = 0;
			cachedIndex[i] = 0x1000000;
		}

		ptrTy = gepInst->getType();
		pointerType = gepInst->getPointerOperandType();

		//createStackNode = new CreateStackNode(gepInst);
		createStackNode = StackNode::getStackNode(gepInst, parentF);
		//baseNode = new GetGeneralNode(gepInst->getOperand(0));
		baseNode = GeneralNode::getGeneralNode(gepInst->getOperand(0), parentF);

		indexOps = SmallVector<Value*, 4>(gepInst->op_begin() + 1, gepInst->op_end());
		realIndexOps = SmallVector<Value*, 4>(gepInst->op_begin() + 1, gepInst->op_end());

		if (gepInst->hasAllConstantIndices()) {
			elemOffset = NodeFactory::dataLayout->
				getIndexedOffset(gepInst->getOperand(0)->getType(), indexOps);
			return;
		}

		// check whether there are non-constant indexes
		unsigned n = indexOps.size();
		PointerType * BaseType = nullptr;
		CompositeType * pointedType = nullptr;
		DS(if (n > 1) {);
			DS(	BaseType = dyn_cast<PointerType>(gepInst->getPointerOperandType()));
			DS(	pointedType = dyn_cast_or_null<CompositeType>(BaseType->getElementType()));
			//DS(	if (!pointedType) {);
			//DS(		errs() << *gepInst << "\n");
			//DS(		errs() << *BaseType << "\n");
			//DS(	});
			DS(});
		for (unsigned i = 0; i < n; ++i) {
			if (i == 0) {
				if (!isa<ConstantInt>(indexOps[0])) {
					nonConstIndexNum++;
					firstIndexNonConstant = true;;
				}
				continue;
			}

			//errs() << "index " << i << "\n";
			//errs() << *pointedType << "\n";
			if (!isa<ConstantInt>(indexOps[i])) {
				nonConstIndexNum++;
#ifdef DUMP_STATISTICS
				ArrayType * aType = dyn_cast<ArrayType>(pointedType);
				if (aType->getNumElements() != 0)
					multi_number *= aType->getNumElements();
				pointedType = dyn_cast_or_null<CompositeType>(aType->getElementType());
			} else {
				pointedType = dyn_cast_or_null<CompositeType>(pointedType->getTypeAtIndex(indexOps[i]));
				//DS(	if (!pointedType) {);
				//DS(		errs() << *gepInst << "\n");
				//DS(		errs() << *BaseType << "\n");
				//DS(	});
#endif
			}
		}

		if (nonConstIndexNum > 0) {
			nonConstIndexIdxes = (unsigned*) calloc(nonConstIndexNum, sizeof(unsigned));
			nonConstIndexes = (int64_t*) calloc(nonConstIndexNum, sizeof(int64_t));
			nonConstIndexes[0] = 0x1000000;
		}

		unsigned j = 0;
		for (unsigned i = 0; i < n; ++i) {
			if (!isa<ConstantInt>(indexOps[i])) {
				nonConstIndexIdxes[j] = i;
				++j;
			}
		}

		if (nonConstIndexNum == 0)
			elemOffset = NodeFactory::dataLayout->
				getIndexedOffset(pointerType, indexOps);
	}

	Node* GetElementPtrNode::act(NodeFactory *nodeFactory) {
		Node *srcNode = baseNode->act(nodeFactory);
		Node *dstNode = createStackNode->act(nodeFactory);

		if (!srcNode || !dstNode)
			llvm_unreachable("srcNode/dstNode is null");

		bool findCached = false;

		for (unsigned i = 0; i < nonConstIndexNum; ++i) {
			int64_t index = getNextPacket() - 4096;
			DS(idx_total++);
			DR((*ofs) << "G: " << index << " - " << global_counter <<  "\n");
			DR(ofs->flush());
			nonConstIndexes[i] = index;
		}

		DN((*ofs) << "============= GetElementPtr Node ================\n");
		DN((*ofs) << *gepInst << "\n");
		if (nonConstIndexNum == 1) {
			for (unsigned i = 0; i < 8; ++i) {
				if (cachedIndex[i] == nonConstIndexes[0]) {
					elemOffset = cachedElementOffset[i];
					findCached = true;
				}
			}
		} 

		unsigned m = 0;
		if(!findCached && nonConstIndexNum > 0) {
			for (unsigned i = 0; i < nonConstIndexNum; ++i) {
				realIndexOps[nonConstIndexIdxes[i]] = 
					ConstantInt::get(Type::getInt32Ty(ptrTy->getContext()), nonConstIndexes[m]);
				m++;
			}
#ifdef DUMP_STATISTICS
			if (firstIndexNonConstant)
				if (nonConstIndexes[0]  + 1 > maxFirstIndex)
					maxFirstIndex = nonConstIndexes[0] + 1;
#endif

			elemOffset = NodeFactory::dataLayout->getIndexedOffset(
					gepInst->getOperand(0)->getType(), realIndexOps);
			if (nonConstIndexNum == 1) {
				cachedIndex[cursor] = nonConstIndexes[0];
				cachedElementOffset[cursor] = elemOffset;
				cursor++;
				if (cursor == 8)
					cursor = 0;
			}
		}

		DN(srcNode->dump());
		DC(srcNode->dump());
		DN((*ofs) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");

		dstNode->mem = srcNode->mem;
		dstNode->startOffset = srcNode->startOffset + elemOffset;

		DN((*ofs) << "elemOffset: " << elemOffset << "\n");

#ifdef DUMP_STATISTICS
		uint64_t tmp;
		if (__builtin_umull_overflow(srcNode->count, multi_number, &tmp))
			dstNode->count = ULONG_MAX;
		else
			dstNode->count = srcNode->count * multi_number;
		if (firstIndexNonConstant) {
			if (__builtin_umull_overflow(dstNode->count, nonConstIndexes[0] + 1, &tmp))
				dstNode->count = ULONG_MAX;
			else
				dstNode->count *= (nonConstIndexes[0] + 1);
		}
		//if (dstNode->count != 1) {
		//	(*ofs) << "  " << "GEPI Node\n";
		//	(*ofs) << "  " << srcNode->count << " * " << multi_number << "\n";
		//	if (firstIndexNonConstant) 
		//		(*ofs) << "  " << maxFirstIndex << "\n";
		//	(*ofs) << "  " << dstNode->count << "\n";
		//}
#endif

		DN(dstNode->dump());
		DC(dstNode->dump());
		DN((*ofs) << "----------------------------------------\n");

		//errs() << "gep update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	PhiNode::PhiNode (const PHINode *phiNode_) : phiNode (phiNode_) {
		bb = phiNode->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		//createStackNode = new CreateStackNode(phiNode);
		createStackNode = StackNode::getStackNode(phiNode, parentF);

		if (phiNode->hasConstantValue()) {
			const Value * inVal = phiNode->getIncomingValue(0);
			constantNodeAction = GeneralNode::getGeneralNode(inVal, parentF);
		} else {
			for (unsigned i = 0; i < phiNode->getNumIncomingValues(); ++i) {
				const Value *inVal = phiNode->getIncomingValue(i);
				const BasicBlock *basicBlock = phiNode->getIncomingBlock(i);
				NodeAction *incomingNode = GeneralNode::getGeneralNode(inVal, parentF);
				BB2PTAAction[basicBlock] = incomingNode;
			}
		}
	}

	Node* PhiNode::act(NodeFactory *nodeFactory) {
		NodeAction * incomingNode = nullptr;

		if (constantNodeAction)
			incomingNode = constantNodeAction;
		else
			incomingNode = BB2PTAAction[PTA::CurrentBlock];

		if (!incomingNode) {
			DRE((*ofs) << "unexpected incoming Block for a PHI node\n");
			DRE((*ofs) << *(PTA::CurrentBlock) << '\n');
			DRE((*ofs) << *phiNode << '\n');

			PTA::dumpCallStack();
		}
		Node *srcNode = incomingNode->act(nodeFactory);
		Node *dstNode = createStackNode->act(nodeFactory);

		if (!srcNode || !dstNode)
			llvm_unreachable("srcNode/dstNode is null");

		DN((*ofs) << "============= PhiNode ======================\n");
		DN((*ofs) << *phiNode << "\n");
		DN(srcNode->dump());
		DC(srcNode->dump());
		DN((*ofs) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");

		srcNode->copy(dstNode);

		DN(dstNode->dump());
		DC(dstNode->dump());
		DN((*ofs) << "---------------------------------------\n");

		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	Node* BitCastNode::act(NodeFactory *nodeFactory) {
		Node *srcNode = fromNode->act(nodeFactory);
		Node *dstNode = createStackNode->act(nodeFactory);

		if (!srcNode || !dstNode)
			llvm_unreachable("srcNode/dstNode is null");

		DN(errs() << "=================== BitCast Node ================\n");
		DN(bitcastInst->dump());
		DN(srcNode->dump());
		DN((*ofs) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");

		srcNode->copy(dstNode);

		DN(dstNode->dump());
		DN(errs() << "----------------------------------------\n");

		//errs() << "BitCast update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	Node* IntToPtrNode::act(NodeFactory *nodeFactory) {
		Node *srcNode = getGeneralNode->act(nodeFactory);
		Node *dstNode = createStackNode->act(nodeFactory);

		if (!srcNode || !dstNode)
			llvm_unreachable("srcNode/dstNode is null");

		srcNode->copy(dstNode);

		//errs() << "IntToPtr update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	Node* PtrToIntNode::act(NodeFactory *nodeFactory) {
		Node *srcNode = getGeneralNode->act(nodeFactory);
		Node *dstNode = createStackNode->act(nodeFactory);

		if (!srcNode || !dstNode)
			llvm_unreachable("srcNode/dstNode is null");

		srcNode->copy(dstNode);

		//errs() << "IntToPtr update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	SelectNode::SelectNode (const SelectInst *selectInst_) : 
		selectInst (selectInst_)
	{
		bb = selectInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		createStackNode = StackNode::getStackNode(selectInst, parentF);
		getGeneralNode[0] = GeneralNode::getGeneralNode(
				selectInst->getFalseValue(), parentF);
		getGeneralNode[1] = GeneralNode::getGeneralNode(
				selectInst->getTrueValue(), parentF);
	}

	Node* SelectNode::act(NodeFactory *nodeFactory) {
		int64_t cond = getNextPacket() - 4096;

		DS(cnd_total++);

		Node *dstNode = createStackNode->act(nodeFactory);
		Node *srcNode = getGeneralNode[cond]->act(nodeFactory);

		if (!(dstNode && srcNode))
			llvm_unreachable("srcNode/dstNode is null");

		DR((*ofs) << "S: " << cond << " - " << global_counter << "\n");
		DR(ofs->flush());

		srcNode->copy(dstNode);

#ifdef DUMP_STATISTICS
		uint64_t tmp;
		if (__builtin_umull_overflow(dstNode->count, 2, &tmp))
			dstNode->count = ULONG_MAX;
		else
			dstNode->count *= 2;
#endif

		//errs() << "Select update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	ExtractElementNode::ExtractElementNode (const ExtractElementInst *extractElementInst_) : extractElementInst (extractElementInst_) {
		bb = extractElementInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		const Value *indexOperand = extractElementInst->getIndexOperand();
		if (!isa<ConstantInt>(indexOperand)) {
			hasNonConstantIndex = true;
			llvm_unreachable("extract element inst has non constant index");
		} else {
			const ConstantInt *cInt = dyn_cast<ConstantInt>(indexOperand);
			elemOffset = cInt->getSExtValue();
		}

		createStackNode = StackNode::getStackNode(extractElementInst, parentF);
		getGeneralNode = GeneralNode::getGeneralNode(extractElementInst->getVectorOperand(), parentF);
	}

	Node* ExtractElementNode::act(NodeFactory *nodeFactory) {
		if (hasNonConstantIndex)
			llvm_unreachable("extract element inst has non constant index");

		Node *srcNode = getGeneralNode->act(nodeFactory);
		Node *dstNode = createStackNode->act(nodeFactory);

		dstNode->mem = srcNode->mem;
		dstNode->startOffset = srcNode->startOffset + elemOffset;
		DS(dstNode->count = srcNode->count);

		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	Node* InsertElementNode::act(NodeFactory *nodeFactory) {
		Node *baseNode = getGeneralNode1->act(nodeFactory);
		Node *valueNode = getGeneralNode2->act(nodeFactory);
		Node *elemNode = baseNode->getNode(elemOffset);
		Node *dstNode = createStackNode->act(nodeFactory);

		valueNode->copy(elemNode);
		baseNode->copy(dstNode);

		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	Node* ExtractValueNode::act(NodeFactory *nodeFactory) {
		if (hasNonConstantIndex)
			llvm_unreachable("extract value inst has non constant index");

		Node *srcNode = getGeneralNode->act(nodeFactory);
		Node *dstNode = createStackNode->act(nodeFactory);

		dstNode->mem = srcNode->mem;
		dstNode->startOffset = srcNode->startOffset + elemOffset;
		DS(dstNode->count = srcNode->count);

		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	Node* InsertValueNode::act(NodeFactory *nodeFactory) {
		Node *baseNode = getGeneralNode1->act(nodeFactory);
		Node *valueNode = getGeneralNode2->act(nodeFactory);
		Node *elemNode = baseNode->getNode(elemOffset);
		Node *dstNode = createStackNode->act(nodeFactory);

		valueNode->copy(elemNode);
		baseNode->copy(dstNode);

		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}

	Node* VAArgNode::act(NodeFactory *nodeFactory) {
		Node *dstNode = createStackNode->act(nodeFactory);
		Node *srcNode = getStackNode->act(nodeFactory);

		if (!srcNode || !dstNode)
			llvm_unreachable("srcNode/dstNode is null");

		srcNode->copy(dstNode);

		//errs() << "VAArg update\n";
		if (shouldUpdateBB)
			PTA::CurrentBlock = bb;

		return nullptr;
	}


	Node* CallbackNode::act(NodeFactory *nodeFactory) {
		/* push start */
		PTA::ShadowBlocks.push_back(PTA::CurrentBlock);

		if (!func)
			llvm_unreachable("push null function");

		PTA::CallStack.push_back(func);
		DCS((*ofs) << "callback push " << func->getName().str() << '\n');
		DCS(ofs->flush());

		vector<NodeAction*> * nodes = Func2Nodes[func];
		for (NodeAction *na : *nodes) {
			na->incCursor();
		}
		/* push end */

		unsigned n = createStackNodes.size();
		for (unsigned i = 0; i < n; ++i) {
			Node *dstNode = createStackNodes[i]->act(nodeFactory);
			dstNode->mem = new NodeMapType ();
			(*(dstNode->mem))[0] = new Node(Node::UNDEFINED, nullptr);
		}
		if (varFuncNode) {
			Node *varArgNode = varFuncNode->act(nodeFactory);
			varArgNode->mem = new NodeMapType ();
			(*(varArgNode->mem))[0] = new Node(Node::UNDEFINED, nullptr);
		}

		return nullptr;
	}


	RetNode::RetNode (const ReturnInst *retInst_) : retInst (retInst_) {
		bb = retInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		if (retInst->getNumOperands() > 0) {
			//getRetNode = new GetGeneralNode(retInst->getOperand(0));
			getRetNode = GeneralNode::getGeneralNode(retInst->getOperand(0), parentF);
		}
	}

	LoadNode::LoadNode (const LoadInst *loadInst_) : loadInst (loadInst_) {
		bb = loadInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		//getGeneralNode = new GetGeneralNode(loadInst->getPointerOperand());
		//createStackNode = new CreateStackNode(loadInst);
		getGeneralNode = GeneralNode::getGeneralNode(loadInst->getPointerOperand(), parentF);
		createStackNode = StackNode::getStackNode(loadInst, parentF);
	}




	BitCastNode::BitCastNode (const BitCastInst *bitcastInst_) : bitcastInst (bitcastInst_) {
		bb = bitcastInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		//createStackNode = new CreateStackNode(bitcastInst);
		//fromNode = new GetGeneralNode(bitcastInst->getOperand(0));
		createStackNode = StackNode::getStackNode(bitcastInst, parentF);
		fromNode = GeneralNode::getGeneralNode(bitcastInst->getOperand(0), parentF);
	}

	IntToPtrNode::IntToPtrNode (const IntToPtrInst *int2ptrInst_) : int2ptrInst (int2ptrInst_) {
		bb = int2ptrInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		//createStackNode = new CreateStackNode(int2ptrInst);
		//getGeneralNode = new GetGeneralNode(int2ptrInst->getOperand(0));
		createStackNode = StackNode::getStackNode(int2ptrInst, parentF);
		getGeneralNode = GeneralNode::getGeneralNode(int2ptrInst->getOperand(0), parentF);
	}

	PtrToIntNode::PtrToIntNode(const PtrToIntInst *ptr2intInst_) : ptr2intInst (ptr2intInst_) {
		bb = ptr2intInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		//createStackNode = new CreateStackNode(ptr2intInst);
		//getGeneralNode = new GetGeneralNode(ptr2intInst->getOperand(0));
		createStackNode = StackNode::getStackNode(ptr2intInst, parentF);
		getGeneralNode = GeneralNode::getGeneralNode(ptr2intInst->getOperand(0), parentF);
	}



	InsertElementNode::InsertElementNode (const InsertElementInst *insertElementInst_) : insertElementInst (insertElementInst_) {
		bb = insertElementInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		const Value *indexOperand = insertElementInst->getOperand(2);
		if (!isa<ConstantInt>(indexOperand)) {
			hasNonConstantIndex = true;
			llvm_unreachable("insert element inst has non constant index");
		} else {
			const ConstantInt *cInt = dyn_cast<ConstantInt>(indexOperand);
			elemOffset = cInt->getSExtValue();
		}

		createStackNode = StackNode::getStackNode(insertElementInst, parentF);
		getGeneralNode1 = GeneralNode::getGeneralNode(insertElementInst->getOperand(0), parentF);
		getGeneralNode2 = GeneralNode::getGeneralNode(insertElementInst->getOperand(1), parentF);
	}


	ExtractValueNode::ExtractValueNode (const ExtractValueInst *extractValueInst_) : extractValueInst (extractValueInst_) {
		bb = extractValueInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		indexOps = SmallVector<Value*, 4>(extractValueInst->op_begin() + 1, extractValueInst->op_end());
		for (unsigned i = 0; i < indexOps.size(); ++i) {
			if (!isa<ConstantInt>(indexOps[i])) {
				hasNonConstantIndex = true;
				llvm_unreachable("extract value inst has non constant index");
			}
		}

		PointerType *ptrTy = PointerType::get(extractValueInst->getOperand(0)->getType(), 0);
		elemOffset = NodeFactory::dataLayout->getIndexedOffset(ptrTy, indexOps);

		createStackNode = StackNode::getStackNode(extractValueInst, parentF);
		getGeneralNode = GeneralNode::getGeneralNode(extractValueInst->getOperand(0), parentF);

	}


	InsertValueNode::InsertValueNode (const InsertValueInst *insertValueInst_) : insertValueInst (insertValueInst_) {
		bb = insertValueInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		indexOps = SmallVector<Value*, 4>(insertValueInst->op_begin() + 2, insertValueInst->op_end());
		for (unsigned i = 0; i < indexOps.size(); ++i) {
			if (!isa<ConstantInt>(indexOps[i])) {
				hasNonConstantIndex = true;
				errs() << *insertValueInst << '\n';
				llvm_unreachable("extract value inst has non constant index");
			}
		}

		PointerType *ptrTy = PointerType::get(insertValueInst->getOperand(0)->getType(), 0);
		elemOffset = NodeFactory::dataLayout->getIndexedOffset(ptrTy, indexOps);

		createStackNode = StackNode::getStackNode(insertValueInst, parentF);
		getGeneralNode1 = GeneralNode::getGeneralNode(insertValueInst->getOperand(0), parentF);
		getGeneralNode2 = GeneralNode::getGeneralNode(insertValueInst->getOperand(1), parentF);
	}

	VAArgNode::VAArgNode (const VAArgInst *vaArgInst_) : vaArgInst (vaArgInst_) {
		bb = vaArgInst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}

		//createStackNode = new CreateStackNode(vaArgInst);
		//getStackNode = new GetStackNode(vaArgInst->getParent()->getParent());
		createStackNode = StackNode::getStackNode(vaArgInst, parentF);
		getStackNode = StackNode::getStackNode(parentF, parentF);
	}

	CallbackNode::CallbackNode(const Function *f): func(f) {
		Function::const_arg_iterator fItr = f->arg_begin();
		while (fItr != f->arg_end()) {
			const Argument* formal = fItr;

			//createStackNodes.push_back(new CreateStackNode(formal));
			createStackNodes.push_back(StackNode::getStackNode(formal, f));

			++fItr;
		}

		if (f->getFunctionType()->isVarArg()) {
			//varFuncNode = new CreateStackNode(f);
			varFuncNode = StackNode::getStackNode(f, f);
		}
	}

	CallbackNode::~CallbackNode() {
		unsigned n = createStackNodes.size();
		for (unsigned i = 0; i < n; ++i) 
			delete createStackNodes[i];

		if (varFuncNode)
			delete varFuncNode;
	}

	DummyNode::DummyNode (const Instruction *inst_) : inst (inst_) {
		bb = inst->getParent();
		parentF = bb->getParent();
		if (parentF->getName().startswith(GLOBAL_CONSTRUCTOR) ||
				parentF->getName().startswith(GLOBAL_VAR)) {
			isInCallback = true;
		}
		dummyNode = NodeFactory::GLOBAL_DUMMY_NODE;
	}

	Node* DummyNode::act(NodeFactory *nodeFactory) {
		return dummyNode;
	}
}
