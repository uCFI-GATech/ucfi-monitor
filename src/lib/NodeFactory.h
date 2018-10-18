#ifndef ANDERSEN_NODE_FACTORY_H
#define ANDERSEN_NODE_FACTORY_H

#include "common.h"

#include "Node.h"
#include "ExternalLibrary.h"

//typedef DenseMap<const Value*, uint64_t> StackFrame;

class NodeFactory
{
	public:
		static const DataLayout* dataLayout;
		static LLVMContext * myContext;
		static DenseMap<const Value*, Node*> GlobalValueNodes;
		static DenseMap<const Value*, vector<Node*>* > HeapNodes;
		static DenseMap<const Value*, vector<Node*>* > StackNodes;

		static Node *GLOBAL_NULL_NODE;
		static Node *GLOBAL_UNDEFINED_NODE;
		static Node *GLOBAL_DUMMY_NODE;
		static vector<Node*> UndefinedNodes;
		static vector<uint64_t> UndefinedNodesNums;

		// save function's current variables' node sizes before calling a function
		//static vector<StackFrame> StackFrames;

		// all functions return undefined values
		static unordered_set<const Function*> ReturnUndefinedValFuns;

		// call stack for saving return values
		vector<Node*> returnValStack;

		void handleGlobals(Module *M);
		uint64_t initGlobal(const Value *V, const Constant *C, uint64_t offset);
		void initGlobal(const Value *V, const Constant *C,
                SmallVector<Value *, 4>& indexOps);
		
		Node* getNodeForConstantExpr(Value *v);

		Node* getGlobalNode(const Value *gVal) {
			DenseMap<const Value*, Node*>::iterator it = GlobalValueNodes.find(gVal);
			assert(it != GlobalValueNodes.end());
			return it->second;
		}
		
		/*
		Node* getStackNode(const Value *sVal) {
			DenseMap<const Value*, vector<Node*>* >::iterator it = StackNodes.find(sVal);
			assert(it != StackNodes.end());
			if (it->second->empty())
				return nullptr;

			vector<Node*> *stackNodes = it->second;
			return (*stackNodes)[stackNodes->size() - 1];
		}

		Node* getHeapNode(const Value *hVal) {
			DenseMap<const Value*, vector<Node*>* >::iterator it = HeapNodes.find(hVal);
			assert(it != HeapNodes.end());
			if (it->second->empty())
				return nullptr;

			vector<Node*> *heapNodes = it->second;
			return (*heapNodes)[heapNodes->size() - 1];
		}

		Node* getNode(const Value *val) {
			if (isa<GlobalValue>(val)) {
				Node *gNode = getGlobalNode(val);
				if (gNode)
					return gNode;
			}
			
			Node *sNode = getStackNode(val);
			if (sNode)
				return sNode;

			Node *hNode = getHeapNode(val);
			if (hNode)
				return hNode;
		}
		*/

		void addGlobalNode(const Value *gVal, uint64_t offset, Node *node) {
			DenseMap<const Value*, Node*>::iterator it = GlobalValueNodes.find(gVal);
			assert(it != GlobalValueNodes.end());
			it->second->insertNode(offset, node);
		}

		static bool isHeapValue(const Value *, const Function *, bool &returnUndefined);

		void dumpMemInfo();

		uint64_t myGetIndexedOffset(Type *ptrTy, ArrayRef<Value *> Indices);
};

#endif
