#include "PTA.h"
#include "PTAAction.h"
#include "KB3Instruction.h"
#include "ControlFlowMonitor.h"

using namespace PTCFI;

SmallVector<KB3Instruction *, 16> KB3Instruction::ExecuteLaterVec;

KB3Instruction::KB3Instruction()
{
}

KB3Instruction::KB3Instruction(const BasicBlock * bb_)
{
	func = bb_->getParent();

	ptaAction = new BasicBlockUpdateNode(bb_);
}

void KB3Instruction::popSomeUntil(uint64_t return_num)
{
	ptaAction->popSomeUntil(return_num);
}

void KB3Instruction::shouldUpdateBB()
{
	ptaAction->shouldUpdateBB = true;
}

KB3Instruction::KB3Instruction(const llvm::Instruction* inst_) 
	: inst (inst_) {
		func = inst->getParent()->getParent();

		switch (inst->getOpcode()) {
			case Instruction::Alloca:
				{
					const AllocaInst* allocaInst = cast<AllocaInst> (inst);
					ptaAction = new AllocateNode(allocaInst);
					break;
				}
			case Instruction::Call:
			case Instruction::Invoke:
				{
					ImmutableCallSite cs(inst);
					calledValue = cs.getCalledValue();
					if (isa<Function>(calledValue)) {
						calledValue = nullptr;
						ptaAction = new CallInvokeNode(cs);
					} else {
						ptaAction = new IndirectCallNode(inst);
						if (const BitCastInst * BCI = dyn_cast<BitCastInst>(calledValue)) {
							calledValue = BCI->getOperand(0);
						}
					}
					break;
				}
			case Instruction::Ret:
				{
					const ReturnInst *retInst = cast<ReturnInst> (inst);
					ptaAction = new RetNode(retInst);
					break;
				}
			case Instruction::Load:
				{
					const LoadInst *loadInst = cast<LoadInst> (inst);
					ptaAction = new LoadNode(loadInst);
					break;
				}
			case Instruction::Store:
				{
					const StoreInst *storeInst = cast<StoreInst> (inst);
					ptaAction = new StoreNode(storeInst);
					break;
				}
			case Instruction::GetElementPtr:
				{
					const GetElementPtrInst *getelementptrInst = cast<GetElementPtrInst> (inst);
					ptaAction = new GetElementPtrNode(getelementptrInst);
					break;
				}
			case Instruction::PHI:
				{
					const PHINode *phiNode = cast<PHINode> (inst);
					ptaAction = new PhiNode(phiNode);
					break;
				}
			case Instruction::BitCast:
				{
					const BitCastInst *bitcastInst = cast<BitCastInst> (inst);
					ptaAction = new BitCastNode(bitcastInst);
					break;
				}
			case Instruction::IntToPtr:
				{
					const IntToPtrInst *inttoptrInst = cast<IntToPtrInst> (inst);
					ptaAction = new IntToPtrNode(inttoptrInst);
					break;
				}
			case Instruction::PtrToInt:
				{
					const PtrToIntInst *ptrtointInst = cast<PtrToIntInst> (inst);
					ptaAction = new PtrToIntNode(ptrtointInst);
					break;
					break;
				}
			case Instruction::Select:
				{
					const SelectInst *selectInst = cast<SelectInst> (inst);
					ptaAction = new SelectNode(selectInst);
					break;
				}
			case Instruction::VAArg:
				{
					const VAArgInst *vaArgInst = cast<VAArgInst> (inst);
					ptaAction = new VAArgNode(vaArgInst);
					break;
				}
			case Instruction::ExtractElement:
				{
					const ExtractElementInst *extractElementInst = cast<ExtractElementInst> (inst);
					ptaAction = new ExtractElementNode(extractElementInst);
					break;
				}
			case Instruction::ExtractValue:
				{
					const ExtractValueInst *extractValueInst = cast<ExtractValueInst> (inst);
					ptaAction = new ExtractValueNode(extractValueInst);
					break;
				}
			case Instruction::InsertElement:
				{
					const InsertElementInst *insertElementInst = cast<InsertElementInst> (inst);
					ptaAction = new InsertElementNode(insertElementInst);
					break;
				}
			case Instruction::InsertValue:
				{
					const InsertValueInst *insertValueInst = cast<InsertValueInst> (inst);
					ptaAction = new InsertValueNode(insertValueInst);
					break;
				}
			default:
				ptaAction = new DummyNode(inst);
				/*
					 inst->dump();
					 llvm_unreachable("unhandled instruction in KB3Instruction\n");
					 */
				break;
		}
	}

void KB3Instruction::execute(PTA* pta) {
	Node * ret = nullptr;
	if (calledValue != nullptr) {
		// get the real address
		uint64_t realAddr = getNextPacket();

		DR((*ofs) << "I: " << HEX(realAddr) << " - " << global_counter << "\n");
		DR(ofs->flush());

		//const Function * validFunc = checkCFI(pta, targetNodeAct);
		const Function * validFunc = cast_or_null<Function>(pta->query(targetNodeAct));

		if (validFunc) {
			if (validFunc != fCache) {
				fCache = validFunc;
				addrCache = AT_Func2Addr[const_cast<Function *>(fCache)];
				indirectNodeCache = ptaAction->getIndirectNode(fCache);
			}

			if (addrCache != realAddr) {
				//DRE((*ofs) << "uCFI fail\n" << *inst << '\n');
				DRE((*ofs) << "uCFI fail\n");
				DS(PTA::fail_count++);
				//DRE((*ofs) << "@bb: " << inst->getParent()->getName() << "\n");
				//DRE((*ofs) << "#function: " << inst->getParent()->getParent()->getName() << "\n");
				//DRE((*ofs) << "expecting " << validFunc->getName() << '@' << HEX(addrCache) << "\n");
				validFunc = AT_Addr2Func[realAddr];
				if (!validFunc) {
					DRE((*ofs) << HEX(realAddr) << " is not a right address\n");
					DRE(ofs->flush());
					llvm_unreachable("");
				} else {
					//DRE((*ofs) << ", but get " << validFunc->getName() << '@' << HEX(realAddr) << '\n');
					//DRE(ofs->flush());
					//llvm_unreachable("CFI fails\n");
				}
				fCache = validFunc;
				addrCache = realAddr;
				indirectNodeCache = ptaAction->getIndirectNode(fCache);
			} 
			else {
				DRE((*ofs) << "uCFI succeed " << HEX(realAddr) << "\n");
				DS(PTA::succeed_count++);
			}
		} else {
			// if no valid func, use the real one
			if (addrCache != realAddr) {
				DRE((*ofs) << "addrCache != realAddr\n");
				fCache = AT_Addr2Func[realAddr];
				addrCache = realAddr;
				indirectNodeCache = ptaAction->getIndirectNode(fCache);
			}

			DRE(if (!fCache) {)
				DRE((*ofs) << HEX(realAddr) << '\n');
				DRE(ofs->flush());
				DRE(llvm_unreachable("null target"));
				DRE(})
					//DRE((*ofs) << "get func: " << fCache->getName() << "\n");

#if 0
					DRE((*ofs)  << *inst << "\n");
				DRE((*ofs) << inst->getParent()->getName() << "\n");
				DRE((*ofs) << inst->getParent()->getParent()->getName() << "\n");
				PTA::dumpCallStack();
				llvm_unreachable("empty");
#endif
		}

		ret = indirectNodeCache->act(pta->nodeFactory);
	} else 
		ret = ptaAction->act(pta->nodeFactory);

	// if is sensitive call, do the following execution after return
	if (ret == (Node *) 1) {
		KB3Instruction::ExecuteLaterVec.push_back(next);
	} else if (next != nullptr) {
		next->execute(pta);
	}
}

KB3CallInvokeInstruction::KB3CallInvokeInstruction(llvm::CallInst* callInst_, llvm::InvokeInst* invokeInst_) : KB3Instruction(callInst_ != nullptr ? (Instruction*) callInst_ : (Instruction*) invokeInst_) {
	AllKB3CallInvokeInstrs.emplace(this);
}
