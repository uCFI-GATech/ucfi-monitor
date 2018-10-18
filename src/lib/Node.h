#ifndef __NODE__H
#define __NODE__H

#include "common.h"

class Node;

typedef DenseMap<int64_t, Node*> NodeMapType;

class Node
{
public:
	// four node types:
	// 	- GLOBAL: global variables, functions
	// 	- STACK: local variables in functions
	// 	- HEAP: objects allocated by calling malloc/calloc
	// 	- UNDEFINED: the node doesn't point to any value
	enum NodeType
	{
		GLOBAL,
		STACK,
		HEAP,
		SPECIAL_STACK,
		UNDEFINED,
		LAZY_FLATTEN,
	};

public:
	NodeType type;
	int64_t startOffset = 0;
	int64_t count = 1;
	const Value *value = nullptr;
	NodeMapType *mem = nullptr;

	Node(NodeType t, const llvm::Value* v = nullptr): 
		type(t), value(v) {
			//if (t == GLOBAL || t == HEAP || t == UNDEFINED) {
			if (t != STACK) {
				mem = new NodeMapType ();
			}
		}

	Node* getNode();
	Node* getNode(int64_t offset);

	void insertNode(int64_t offset, Node *node);
	void insertNode(Node *node);

	void init() {
		// TODO: preinitialize mem, improve performance
	}
	
	// copy this's mem to dstNode's mem
	void copy(Node *dstNode);
	void deepCopy(Node *, int64_t);

	void dump();
	void dump(std::unordered_set<NodeMapType *> &, unsigned);
};

#endif
