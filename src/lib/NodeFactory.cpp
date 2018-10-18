#include "common.h"
#include "NodeFactory.h"
#include "Flags.h"

LLVMContext * NodeFactory::myContext = nullptr;
const DataLayout* NodeFactory::dataLayout = nullptr;
DenseMap<const Value*, Node*> NodeFactory::GlobalValueNodes;
DenseMap<const Value*, vector<Node*>* > NodeFactory::HeapNodes;
DenseMap<const Value*, vector<Node*>* > NodeFactory::StackNodes;

Node * NodeFactory::GLOBAL_NULL_NODE = new Node(Node::UNDEFINED, nullptr);
Node * NodeFactory::GLOBAL_UNDEFINED_NODE = new Node(Node::UNDEFINED, nullptr);
Node * NodeFactory::GLOBAL_DUMMY_NODE = new Node(Node::UNDEFINED, nullptr);

vector<Node*> NodeFactory::UndefinedNodes;
vector<uint64_t> NodeFactory::UndefinedNodesNums;

//vector<StackFrame> NodeFactory::StackFrames;

unordered_set<const Function*> NodeFactory::ReturnUndefinedValFuns;

void NodeFactory::handleGlobals(Module *M) {
	// global variables
	for (auto const& globalVal : M->globals()) {
		pair<const Value*, Node*> p (&globalVal, new Node(Node::GLOBAL, &globalVal));
		GlobalValueNodes.insert(p);
	}
	
	// functions
	for (auto const& f : *M) {
		if (f.hasAddressTaken()) {
			pair<const Value*, Node*> p (&f, new Node(Node::GLOBAL, &f));
			GlobalValueNodes.insert(p);
			p.second->mem->operator[](0) = new Node(Node::GLOBAL, &f);
		}
	}

	// global alias
	for (auto const& globalAlias : M->aliases()) {
		Node *gNode = getGlobalNode(globalAlias.getBaseObject());
		pair<const Value*, Node*> p (&globalAlias, gNode);
		GlobalValueNodes.insert(p);
	}
	
	// initialize the globals
	for (auto const& globalVal: M->globals()) {
		const Type *type = globalVal.getType()->getElementType();
		
		// skip i8 array
		if (const ArrayType *arrayType = dyn_cast<ArrayType>(type)) {
			if (arrayType->getElementType()->getPrimitiveSizeInBits() == 8) {
				continue;
			}
		}

		if (globalVal.hasDefinitiveInitializer()) {
            SmallVector<Value *, 4> indexOps;
            IntegerType * Int32Ty = IntegerType::getInt32Ty(*myContext);
            ConstantInt * CI = ConstantInt::get(Int32Ty, 0);
            indexOps.push_back(CI);
			initGlobal(&globalVal, globalVal.getInitializer(), indexOps);
		}
	}
    
    // print out global nodes
	/*
    for (auto globalValueNode : GlobalValueNodes) {
    //    if (globalValueNode.second->value->getName() == "c_format_attribute_table")
		if (isa<Function>(globalValueNode.first))
			continue;

	    errs() << *globalValueNode.first << "\n";
		if (globalValueNode.second)
	        globalValueNode.second->dump();
		else
			errs() << "null\n";
	}

	//exit(0)
	*/

	// get all functions return undefined values
	for (Module::iterator it = M->begin(), ie = M->end(); it != ie; ++it) {
		Function &f = *it;
		if (f.isDeclaration() || f.isIntrinsic()) {
			continue;
		}

    bool allReturnUndefined = true;
		for (inst_iterator ii = inst_begin(f), ie = inst_end(f);
				ii != ie; ++ii) {
			Instruction *inst = &(*ii);
			if (ReturnInst *retInst = dyn_cast<ReturnInst>(inst)) {
				if (retInst->getMetadata("is-sensitive")) {
          allReturnUndefined = false;
				}
			}
		}
    if (allReturnUndefined) {
		  ReturnUndefinedValFuns.insert(&f);
    }
	}
}

// for handling embedded constant expr in global variables
Node* NodeFactory::getNodeForConstantExpr(Value *v) {
	if (GlobalValue *gVal = dyn_cast<GlobalValue>(v)) {
		Node *gNode = getGlobalNode(v);
		if (!gNode)
			llvm_unreachable("cannot get node for global value");
		return gNode;
	}

	ConstantExpr *ce = dyn_cast<ConstantExpr>(v);
	if (!ce) {
		llvm_unreachable("not constant expr");
	}
	switch (ce->getOpcode()) {
		case Instruction::GetElementPtr:
			{
				Node *gNode = nullptr;

				GlobalVariable *gVal = dyn_cast<GlobalVariable>(ce->getOperand(0));
				if (gVal) {
					gNode = getGlobalNode(gVal);
				} else  {
					gNode = getNodeForConstantExpr(ce->getOperand(0));
				}

				if (!gNode)
					llvm_unreachable("cannot get node for gep");

				SmallVector<Value*, 4> indexOps(ce->op_begin() + 1, ce->op_end());
				int64_t elemOffset = dataLayout->getIndexedOffset(ce->getOperand(0)->getType(), indexOps);
				// elemNode = gNode->mem[elemOffset]

				Node *newNode = new Node(Node::GLOBAL, nullptr);
				newNode->mem = gNode->mem;
				newNode->startOffset = gNode->startOffset + elemOffset;

				DS(newNode->count = gNode->count);

				Node *elemNode = gNode->getNode(elemOffset);
				if (elemNode == nullptr) {
					elemNode = new Node(Node::UNDEFINED, nullptr);
					gNode->insertNode(elemOffset, elemNode);
				}

				return newNode;

			}
		case Instruction::BitCast:
			{
					Node *gNode = nullptr;
					GlobalVariable *gVal = dyn_cast<GlobalVariable>(ce->getOperand(0));
					if (gVal) {
						gNode = getGlobalNode(gVal);
					} else {
						gNode = getNodeForConstantExpr(ce->getOperand(0));
					}
					return gNode;
			}
		case Instruction::PtrToInt:
        case Instruction::IntToPtr:
			{
				Node *newNode = new Node(Node::UNDEFINED, nullptr);
				return newNode;
			}
		default:
			llvm_unreachable("not getelementptr/bitcast");
			break;
	}
	llvm_unreachable("end of getNodeForConstantExpr");
	return nullptr;
}

void NodeFactory::initGlobal(const Value *V, const Constant *C, 
        SmallVector<Value *, 4>& indexOps) {
    uint64_t offset = 0;
	if (const BlockAddress *bAddr = dyn_cast<BlockAddress>(C)) {
		llvm_unreachable("unhandled global block address");
	}	else if (const ConstantAggregateZero *caz = dyn_cast<ConstantAggregateZero>(C)) {
		// don't need to initialize zero initializer
        return;
	} else if (const ConstantArray *ca = dyn_cast<ConstantArray>(C)) {
		unsigned opNum = ca->getNumOperands();
		for (unsigned i = 0; i < opNum; ++i) {
            IntegerType * Int32Ty = IntegerType::getInt32Ty(*myContext);
            ConstantInt * CI = ConstantInt::get(Int32Ty, i);
            indexOps.push_back(CI);
			initGlobal(V, ca->getOperand(i), indexOps);
            indexOps.pop_back();
		}
		return;
	} else if (const ConstantDataSequential *cds = dyn_cast<ConstantDataSequential>(C)) {
		unsigned elemNum = cds->getNumElements();
		for (unsigned i = 0; i < elemNum; ++i) {
            IntegerType * Int32Ty = IntegerType::getInt32Ty(*myContext);
            ConstantInt * CI = ConstantInt::get(Int32Ty, i);
            indexOps.push_back(CI);
			initGlobal(V, cds->getElementAsConstant(i), indexOps);
            indexOps.pop_back();
		}
		return;
	} else if (const ConstantExpr *ce = dyn_cast<ConstantExpr>(C) ) {
        offset = dataLayout->getIndexedOffset(V->getType(), indexOps);
		switch (ce->getOpcode()) {
			case Instruction::GetElementPtr:
				{
					Node *elemNode = getNodeForConstantExpr(const_cast<ConstantExpr*>(ce));
					addGlobalNode(V, offset, elemNode);
					break;
				}
			case Instruction::BitCast:
				{
					Node *gNode = getNodeForConstantExpr(const_cast<ConstantExpr*>(ce));
					addGlobalNode(V, offset, gNode);
					break;
				}
			case Instruction::PtrToInt:
            case Instruction::IntToPtr:
				{
					Node *gNode = getNodeForConstantExpr(const_cast<ConstantExpr*>(ce));
					addGlobalNode(V, offset, gNode);
					break;
				}
			default:
				errs() << ce->getOpcodeName() << '\n';
				llvm_unreachable("unhandled constant expession in global variable initialization");
		}
		return;
	} else if (const ConstantFP *cfp = dyn_cast<ConstantFP>(C)) {
		// don't need to initialize float pointing number
		return;
	} else if (const ConstantInt *ci = dyn_cast<ConstantInt>(C)) {
		// don't need to initialize int
		return;
	} else if (const ConstantPointerNull *cpn = dyn_cast<ConstantPointerNull>(C)) {
		// intialize to undefined node
        offset = dataLayout->getIndexedOffset(V->getType(), indexOps);
		addGlobalNode(V, offset, new Node(Node::UNDEFINED, nullptr));
		return;
	} else if (const ConstantStruct *cs = dyn_cast<ConstantStruct>(C)) {
		unsigned opNum = cs->getNumOperands();
		for (unsigned i = 0; i < opNum; ++i) {
            IntegerType * Int32Ty = IntegerType::getInt32Ty(*myContext);
            ConstantInt * CI = ConstantInt::get(Int32Ty, i);
            indexOps.push_back(CI);
			initGlobal(V, cs->getOperand(i), indexOps);
            indexOps.pop_back();
		}
		return;
	} else if (const ConstantVector *cv = dyn_cast<ConstantVector>(C)) {
		unsigned opNum = cv->getNumOperands();
		for (unsigned i = 0; i < opNum; ++i) {
            IntegerType * Int32Ty = IntegerType::getInt32Ty(*myContext);
            ConstantInt * CI = ConstantInt::get(Int32Ty, i);
            indexOps.push_back(CI);
			initGlobal(V, cs->getOperand(i), indexOps);
            indexOps.pop_back();
		}
		return;
	} else if (const GlobalValue *gv = dyn_cast<GlobalValue>(C)) {
		// add the node
		Node *gNode = getGlobalNode(gv);
		// GlobalNodes[V]->mem[offset] = gNode
        offset = dataLayout->getIndexedOffset(V->getType(), indexOps);
		addGlobalNode(V, offset, gNode);
		return;
	} else if (const UndefValue *uv = dyn_cast<UndefValue>(C)) {
		// intialize to undefined node
        offset = dataLayout->getIndexedOffset(V->getType(), indexOps);
		addGlobalNode(V, offset, new Node(Node::UNDEFINED, nullptr));
		return;
	}

	llvm_unreachable("not handled global initialization");
	return;
}
uint64_t NodeFactory::initGlobal(const Value *V, const Constant *C, uint64_t offset) {
	if (const BlockAddress *bAddr = dyn_cast<BlockAddress>(C)) {
		llvm_unreachable("unhandled global block address");
	}	else if (const ConstantAggregateZero *caz = dyn_cast<ConstantAggregateZero>(C)) {
		// don't need to initialize zero initializer
		uint64_t tySize = dataLayout->getTypeAllocSize(caz->getType());
		// update offset
		return (offset + tySize);
	} else if (const ConstantArray *ca = dyn_cast<ConstantArray>(C)) {
		unsigned opNum = ca->getNumOperands();
		for (unsigned i = 0; i < opNum; ++i) {
			// update offset
			offset = initGlobal(V, ca->getOperand(i), offset);
		}
		return offset;
	} else if (const ConstantDataSequential *cds = dyn_cast<ConstantDataSequential>(C)) {
		unsigned elemNum = cds->getNumElements();
		for (unsigned i = 0; i < elemNum; ++i) {
			// update offset
			offset = initGlobal(V, cds->getElementAsConstant(i), offset);
		}
		return offset;
	} else if (const ConstantExpr *ce = dyn_cast<ConstantExpr>(C) ) {
		switch (ce->getOpcode()) {
			case Instruction::GetElementPtr:
				{
					Node *elemNode = getNodeForConstantExpr(const_cast<ConstantExpr*>(ce));
					addGlobalNode(V, offset, elemNode);
					// update offset
					offset += dataLayout->getTypeAllocSize(ce->getType());
					break;
				}
			case Instruction::BitCast:
				{
					Node *gNode = getNodeForConstantExpr(const_cast<ConstantExpr*>(ce));
					addGlobalNode(V, offset, gNode);
					// update offset
					offset += dataLayout->getTypeAllocSize(ce->getType());
					break;
				}
			case Instruction::PtrToInt:
				{
					Node *gNode = getNodeForConstantExpr(const_cast<ConstantExpr*>(ce));
					addGlobalNode(V, offset, gNode);
					// update offset
					offset += dataLayout->getTypeAllocSize(ce->getType());
					break;
				}
			default:
				errs() << ce->getOpcodeName() << '\n';
				llvm_unreachable("unhandled constant expession in global variable initialization");
		}
		return offset;
	} else if (const ConstantFP *cfp = dyn_cast<ConstantFP>(C)) {
		// don't need to initialize float pointing number
		uint64_t tySize = dataLayout->getTypeAllocSize(cfp->getType());
		// update offset
		return (offset + tySize);
	} else if (const ConstantInt *ci = dyn_cast<ConstantInt>(C)) {
		// don't need to initialize int
		uint64_t tySize = dataLayout->getTypeAllocSize(ci->getType());
		// update offset
		return (offset + tySize);
	} else if (const ConstantPointerNull *cpn = dyn_cast<ConstantPointerNull>(C)) {
		// intialize to undefined node
		addGlobalNode(V, offset, new Node(Node::UNDEFINED, nullptr));
		uint64_t tySize = dataLayout->getTypeAllocSize(cpn->getType());
		// update offset
		return (offset + tySize);
	} else if (const ConstantStruct *cs = dyn_cast<ConstantStruct>(C)) {
		unsigned opNum = cs->getNumOperands();
		for (unsigned i = 0; i < opNum; ++i) {
			offset = initGlobal(V, cs->getOperand(i), offset);
		}
		return offset;
	} else if (const ConstantVector *cv = dyn_cast<ConstantVector>(C)) {
		unsigned opNum = cv->getNumOperands();
		for (unsigned i = 0; i < opNum; ++i) {
			offset = initGlobal(V, cv->getOperand(i), offset);
		}
		return offset;
	} else if (const GlobalValue *gv = dyn_cast<GlobalValue>(C)) {
		// add the node
		Node *gNode = getGlobalNode(gv);
		// GlobalNodes[V]->mem[offset] = gNode
		addGlobalNode(V, offset, gNode);
		// update offset
		uint64_t tySize = dataLayout->getTypeAllocSize(gv->getType());
		return (offset + tySize);
	} else if (const UndefValue *uv = dyn_cast<UndefValue>(C)) {
		// intialize to undefined node
		addGlobalNode(V, offset, new Node(Node::UNDEFINED, nullptr));
		uint64_t tySize = dataLayout->getTypeAllocSize(uv->getType());
		// update offset
		return (offset + tySize);
	}

	llvm_unreachable("not handled global initialization");
	return 0;
}

bool NodeFactory::isHeapValue(const Value *val, 
		const Function * calledFunc, bool &returnUndefined) {
	returnUndefined = false;

	if (!calledFunc) {
		if (const CallInst *cInst = dyn_cast<CallInst>(val)) {
			calledFunc = cInst->getCalledFunction();
		} else if (const InvokeInst *iInst = dyn_cast<InvokeInst>(val)) {
			calledFunc = iInst->getCalledFunction();
		}
	}

	if (!calledFunc) {
		return false;
	}

	if (NodeFactory::ReturnUndefinedValFuns.find(calledFunc) !=
			NodeFactory::ReturnUndefinedValFuns.end()) {
		returnUndefined = true;
	}
	
	const char *fName = calledFunc->getName().data();
	if (ExternalLibrary::lookupName(ExternalLibrary::mallocFuncs, fName) 
		||
		ExternalLibrary::lookupName(ExternalLibrary::reallocFuncs, fName)
		||
		(returnUndefined && 
		 !cast<Instruction>(val)->getMetadata("is-less-sensitive"))) 
	{
		return true;
	}

	return false;
}

void NodeFactory::dumpMemInfo() {
	errs() << "global node size: " << GlobalValueNodes.size() << '\n';
	DenseMap<const Value*, vector<Node*>* >::iterator it = HeapNodes.begin(), ie = HeapNodes.end();

	uint64_t heapNodeNum = 0;
	for (; it != ie; ++it) {
		heapNodeNum += it->second->size();
	}
	errs() << "heap node size: " << heapNodeNum << '\n';
	
	uint64_t stackNodeNum = 0;
	it = StackNodes.begin(), ie = StackNodes.end();
	for (; it != ie; ++it) {
		stackNodeNum += it->second->size();
	}
	errs() << "stack node size: " << stackNodeNum << '\n';
}

uint64_t NodeFactory::myGetIndexedOffset(Type *ptrTy, ArrayRef<Value *> Indices) {
  Type *Ty = ptrTy;
  assert(Ty->isPointerTy() && "Illegal argument for getIndexedOffset()");
  uint64_t Result = 0;

  generic_gep_type_iterator<Value* const*>
    TI = gep_type_begin(ptrTy, Indices);
  for (unsigned CurIDX = 0, EndIDX = Indices.size(); CurIDX != EndIDX;
       ++CurIDX, ++TI) {
    if (StructType *STy = dyn_cast<StructType>(*TI)) {
      assert(Indices[CurIDX]->getType() ==
             Type::getInt32Ty(ptrTy->getContext()) &&
             "Illegal struct idx");
      unsigned FieldNo = cast<ConstantInt>(Indices[CurIDX])->getZExtValue();

      // Get structure layout information...
      const StructLayout *Layout = dataLayout->getStructLayout(STy);

      // Add in the offset, as calculated by the structure layout info...
      Result += Layout->getElementOffset(FieldNo);

      // Update Ty to refer to current element
      Ty = STy->getElementType(FieldNo);
    } else {
      // Update Ty to refer to current element
      Ty = cast<SequentialType>(Ty)->getElementType();

      // Get the array index and the size of each array element.
      if (int64_t arrayIdx = cast<ConstantInt>(Indices[CurIDX])->getSExtValue())
        Result += (uint64_t)arrayIdx * dataLayout->getTypeAllocSize(Ty);
    }
  }

  return Result;
}
