#ifndef __KB3_PTAACTION__
#define __KB3_PTAACTION__

#include "common.h"

#include "NodeFactory.h"

namespace PTCFI {

#define NODE_INIT_SIZE 1
#define INC_SIZE 4096
#define GLOBAL_CONSTRUCTOR "_GLOBAL__sub_I_"
#define GLOBAL_VAR "__cxx_global"

	class NodeAction {
		public:
			bool shouldUpdateBB;
			const Value *val = nullptr;
			const BasicBlock * bb = nullptr;
			const Function *parentF = nullptr;

			bool isInCallback = false;
			// for stack & heap node 
			Node **nodes = nullptr;
			unsigned nodeSize = 0;
			int cursor = -1;
			Node::NodeType type;
			
			NodeAction() : shouldUpdateBB(false) {}	
			NodeAction (const Value *val_) : 
				val (val_), shouldUpdateBB(false) {}

			virtual NodeAction* getIndirectNode(const Function *func_) {
				return nullptr;
			}
			virtual Node* act(NodeFactory *nodeFactory);
			virtual ~NodeAction() {}

			void incCursor();
			void decCursor();

			void popSomeUntil(uint64_t);
	};

	class FunctionNode : public NodeAction {
		private:
			Node * node;

		public:
			FunctionNode(const Function * func);
			Node * act(NodeFactory *nodeFactory);
	};

	class BasicBlockUpdateNode : public NodeAction {
		public:
			BasicBlockUpdateNode(const BasicBlock *bb_);
			Node* act(NodeFactory *nodeFactory);
	};

	class UndefinedNode : public NodeAction {
		public:
			static UndefinedNode *instance;
			static UndefinedNode *getUndefinedNode(const Value *val_);
			Node* act(NodeFactory *nodeFactory);

		private:
			UndefinedNode(const Value *val_) : NodeAction(val_) {}
	};

	class NullNode : public NodeAction {
		public:
			static NullNode *instance;
			static NullNode *getNullNode(const Value *val_);
			Node* act(NodeFactory *nodeFactory);

		private:
			NullNode(const Value *val_) : NodeAction(val_) {}
	};
	
	extern DenseMap<const Function*, vector<NodeAction*> *> Func2Nodes;
	class StackNode : public NodeAction {
		public:
			bool isUndefinedVal = false;
			bool isNullVal = false;
			bool isAllocate = false;
			static DenseMap<const Value*, NodeAction*> StackNodeMap;

			Node* act(NodeFactory *nodeFactory);
			static NodeAction * getStackNode(const Value *val_, const Function *func_);

		private:
			StackNode(const Value *val_, const Function *func_);
	};
	
	class HeapNode : public NodeAction {
		public:
			bool isUndefinedVal = false;
			bool isNullVal = false;
			static DenseMap<const Value*, HeapNode*> HeapNodeMap;

			Node* act(NodeFactory *nodeFactory);
			static HeapNode * getHeapNode(const Value *val_, const Function *func_);
			HeapNode * forceCreateHeapNode(const Value *val_, const Function *func_);

		private:
			HeapNode (const Value *val_, const Function *func_);
	};

	class GlobalNode : public NodeAction {
		public:
			Node *node = nullptr;
			static DenseMap<const Value*, GlobalNode*> GlobalNodeMap;
			static GlobalNode * getGlobalNode(const Value *val_);
			Node* act(NodeFactory *nodeFactory);

		private:
			GlobalNode (const Value *val_);
	};

	class GeneralNode : public NodeAction {
		public:
			static NodeAction * getGeneralNode(const Value *val, const Function* func);
	};

	class InitArgsNode : public NodeAction {
		public:
			const Function *caller;
			const Function *target;
			unsigned actualArgNodesNum = 0;
			vector<NodeAction*> actualArgNodes;
			vector<NodeAction*> formalArgNodes;
			NodeAction *varFuncNode = nullptr;
			vector<NodeAction*> varArgNodes;

			bool allUndefinedArgs = true;

			~InitArgsNode();
			InitArgsNode (ImmutableCallSite cs, const Function *target_, 
					const Function *caller_);
			Node* act(NodeFactory *nodeFactory) { llvm_unreachable("never call this"); }
			Node* act(NodeFactory *nodeFactory, Node **srcNodes, Node **varSrcNodes);
	};

	class ExternalCallNode : public NodeAction {
		public:
			int actionType = 0;
			int copySize = 0;
			HeapNode *createHeapNode = nullptr;
			// TODO: remove stack node?
			NodeAction *createStackNode = nullptr;
			
			NodeAction *getArg0Node = nullptr;
			NodeAction *getArg1Node = nullptr;
			NodeAction *getArg2Node = nullptr;
			NodeAction *getVarArgNode = nullptr;
			vector<NodeAction*> getArgNodes;
			const Instruction * inst;

			~ExternalCallNode();
			ExternalCallNode(ImmutableCallSite cs, const Function* f);
			Node* act(NodeFactory *nodeFactory);
	};

	class AllocateNode : public NodeAction {
		public:
			const AllocaInst *allocaInst = nullptr;
			NodeAction *createStackNode = nullptr;

			AllocateNode (const AllocaInst *allocaInst_);
			Node* act(NodeFactory *nodeFactory);
	};
	
	class IndirectCallNode : public NodeAction {
		public:
			const Instruction *inst = nullptr;
			const Function *func = nullptr;

			ExternalCallNode *externalCallNode = nullptr;
			InitArgsNode *initArgsNode = nullptr;
			NodeAction *retStackNode = nullptr;
			HeapNode *retHeapNode = nullptr;
			bool isTargetSensive = false;
			vector<NodeAction *> * nodes = nullptr;
			DenseMap<const Function*, IndirectCallNode*> ActivatedTargets;
			Node **srcNodes = nullptr;
			Node **argSrcNodes = nullptr;
			
			IndirectCallNode(const Instruction *inst_);
			IndirectCallNode(const Instruction *inst_, const Function *func_);
			NodeAction* getIndirectNode(const Function *func);
			Node* act(NodeFactory *nodeFactory);
			~IndirectCallNode();
	};

	class CallInvokeNode : public NodeAction {
		public:
			const Instruction *inst = nullptr;
			const Function *targetFunc = nullptr;

			ExternalCallNode *externalCallNode = nullptr;
			InitArgsNode *initArgsNode = nullptr;
			NodeAction *retStackNode = nullptr;
			HeapNode *retHeapNode = nullptr;
			bool isTargetSensive = false;
			vector<NodeAction *> *nodes = nullptr;
			IndirectCallNode *indirectCallNode = nullptr;
			Node **srcNodes = nullptr;
			Node **argSrcNodes = nullptr;

			CallInvokeNode (ImmutableCallSite cs);
			Node* act(NodeFactory *nodeFactory);
	};

	class RetNode : public NodeAction {
		public:
			const ReturnInst *retInst = nullptr;
			NodeAction *getRetNode = nullptr;

			RetNode (const ReturnInst *retInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class LoadNode : public NodeAction {
		public:
			const LoadInst *loadInst = nullptr;
			NodeAction *createStackNode = nullptr;
			NodeAction *getGeneralNode = nullptr;

			LoadNode (const LoadInst *loadInst_);
			Node* act(NodeFactory *nodeFactory);
	};


	class StoreNode : public NodeAction {
		public:
			const StoreInst *storeInst = nullptr;
			NodeAction *srcGetGeneralNode = nullptr;
			NodeAction *dstGetGeneralNode = nullptr;

			StoreNode (const StoreInst *storeInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class GetElementPtrNode : public NodeAction {
		public:
			const GetElementPtrInst *gepInst = nullptr;
			NodeAction *createStackNode = nullptr;
			NodeAction *baseNode = nullptr;
			
			Type *ptrTy = nullptr;
			Type *pointerType = nullptr;
			SmallVector<Value*, 4> indexOps;
			SmallVector<Value*, 4> realIndexOps;
			int64_t elemOffset = 0;
			bool hasNonConstIndex = false;
			unsigned nonConstIndexNum = 0;
			unsigned *nonConstIndexIdxes = nullptr;

			bool firstIndexNonConstant = false;
			int64_t maxFirstIndex = 2;
			int64_t multi_number = 1;

			//cache one non constant index, which is very common
			unsigned cursor = 0;
			int64_t cachedElementOffset[8];
			int64_t cachedIndex[8];

			//for holding non constant indexes
			int64_t *nonConstIndexes;
			
			GetElementPtrNode (const GetElementPtrInst *gepInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class PhiNode : public NodeAction {
		public:
			const PHINode *phiNode = nullptr;
			NodeAction *createStackNode = nullptr;
			DenseMap<const BasicBlock *, NodeAction *> BB2PTAAction;
			NodeAction * constantNodeAction = nullptr;

			PhiNode (const PHINode *phiNode_);
			Node* act(NodeFactory *nodeFactory);
	};

	class BitCastNode : public NodeAction {
		public:
			const BitCastInst *bitcastInst = nullptr;
			NodeAction *createStackNode = nullptr;
			NodeAction *fromNode = nullptr;
			
			BitCastNode (const BitCastInst *bitcastInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class IntToPtrNode : public NodeAction {
		public:
			const IntToPtrInst *int2ptrInst = nullptr;
			NodeAction *createStackNode = nullptr;
			NodeAction *getGeneralNode = nullptr;

			IntToPtrNode (const IntToPtrInst *int2ptrInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class PtrToIntNode : public NodeAction {
		public:
			const PtrToIntInst *ptr2intInst = nullptr;
			NodeAction *createStackNode = nullptr;
			NodeAction *getGeneralNode = nullptr;

			PtrToIntNode(const PtrToIntInst *ptr2intInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class SelectNode : public NodeAction {
		public:
			const SelectInst *selectInst = nullptr;
			NodeAction *createStackNode = nullptr;
			NodeAction *getGeneralNode[2] = {nullptr, nullptr};

			SelectNode (const SelectInst *selectInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class ExtractElementNode : public NodeAction {
		public:
			const ExtractElementInst *extractElementInst = nullptr;
			NodeAction *getGeneralNode = nullptr;
			NodeAction *createStackNode = nullptr;
			int64_t elemOffset = 0;
			bool hasNonConstantIndex = false;

			ExtractElementNode (const ExtractElementInst *extractElementInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class InsertElementNode : public NodeAction {
		public:
			const InsertElementInst *insertElementInst = nullptr;
			NodeAction *getGeneralNode1 = nullptr;
			NodeAction *getGeneralNode2 = nullptr;
			NodeAction *createStackNode = nullptr;
			int64_t elemOffset = 0;
			bool hasNonConstantIndex = false;

			InsertElementNode (const InsertElementInst *insertElementInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class ExtractValueNode : public NodeAction {
		public:
			const ExtractValueInst *extractValueInst = nullptr;
			NodeAction *getGeneralNode = nullptr;
			NodeAction *createStackNode = nullptr;
			int64_t elemOffset = 0;
			bool hasNonConstantIndex = false;
			SmallVector<Value*, 4> indexOps;

			ExtractValueNode (const ExtractValueInst *extractValueInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class InsertValueNode : public NodeAction {
		public:
			const InsertValueInst *insertValueInst = nullptr;
			NodeAction *getGeneralNode1 = nullptr;
			NodeAction *getGeneralNode2 = nullptr;
			NodeAction *createStackNode = nullptr;
			int64_t elemOffset = 0;
			bool hasNonConstantIndex = false;
			SmallVector<Value*, 4> indexOps;

			InsertValueNode (const InsertValueInst *insertValueInst_);
			Node* act(NodeFactory *nodeFactory);
	};


	class VAArgNode : public NodeAction {
		public:
			const VAArgInst *vaArgInst = nullptr;
			NodeAction *createStackNode = nullptr;
			NodeAction *getStackNode = nullptr;

			VAArgNode (const VAArgInst *vaArgInst_);
			Node* act(NodeFactory *nodeFactory);
	};

	class CallbackNode : public NodeAction {
		public:
			const Function* func = nullptr;
			vector<NodeAction*> createStackNodes;
			NodeAction *varFuncNode = nullptr;

			CallbackNode(const Function *f);
			~CallbackNode();
			Node* act(NodeFactory *nodeFactory);
	};

	class DummyNode : public NodeAction {
		public:
			const Instruction *inst = nullptr;
			Node* dummyNode = nullptr;

			DummyNode (const Instruction *inst_);
			Node* act(NodeFactory *nodeFactory);
	};

}

#endif
