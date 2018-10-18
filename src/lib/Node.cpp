#include "Node.h"
#include "ControlFlowMonitor.h"

using namespace PTCFI;

// copy this's mem to dstNode's mem
void Node::copy(Node *dstNode) {
	if (!this->mem) {
		DRE(((*ofs) << "null mem\n"));
		DRE(this->dump());

		this->mem = new NodeMapType ();
	}

	dstNode->mem = this->mem;
	dstNode->startOffset = this->startOffset;
	DS(dstNode->count = this->count);
}

void Node::deepCopy(Node * dstNode, int64_t copySize) {
	//(*ofs) << "deepCopy " << copySize << "\n";
	if (!this->mem) {
		DRE(((*ofs) << "null mem\n"));
		DRE(this->dump());
		this->mem = new NodeMapType ();
	}

	if (!dstNode->mem)
		dstNode->mem = new NodeMapType ();

	// to avoid breaking the iterator when copy to the same map
	NodeMapType toUpdate;
	bool copyToThis = (dstNode->mem == this->mem);
	NodeMapType * toUpdateMap = &toUpdate;
	NodeMapType * dstMap = dstNode->mem;

	// which iteration strategy to use
	int64_t copyPointerNum = copySize >> 3;
	bool iteratorAll = (copyPointerNum > this->mem->size());

	int64_t offset_diff = dstNode->startOffset - this->startOffset;
	int64_t start_copy  = this->startOffset;
	int64_t end_copy    = start_copy + copySize;

	if (iteratorAll) {
		for (auto iter = this->mem->begin(); iter != this->mem->end(); iter++)  {
			int64_t field_offset = iter->first;
			if (field_offset >= start_copy && field_offset < end_copy) {
				int64_t target_offset = field_offset + offset_diff;
				auto dst_iter = dstMap->find(target_offset);
				if (dst_iter == dstMap->end()) {
					Node *newNode = new Node(Node::LAZY_FLATTEN, nullptr);
					toUpdateMap->operator[](target_offset) = newNode;
					iter->second->copy(newNode);;
				} else {
					iter->second->copy(dst_iter->second);
				}
			}
		}
	} else {
		for (auto field_offset = start_copy; field_offset < end_copy; field_offset += 8) {
			auto iter = this->mem->find(field_offset);
			if (iter != this->mem->end()) {
				int64_t target_offset = field_offset + offset_diff;
				auto dst_iter = dstMap->find(target_offset);
				if (dst_iter == dstMap->end()) {
					Node *newNode = new Node(Node::LAZY_FLATTEN, nullptr);
					toUpdateMap->operator[](target_offset) = newNode;
					iter->second->copy(newNode);;
				} else {
					iter->second->copy(dst_iter->second);
				}
			}
		}
	}

	if (copyToThis)
		for (auto iter = toUpdate.begin(); iter != toUpdate.end(); iter++) {
			int64_t target_offset = iter->first;
			dstNode->mem->operator[](target_offset) = iter->second;
		}

	DS(dstNode->count = this->count);
}

Node* Node::getNode(int64_t offset) {
	int64_t realOffset = startOffset + offset;

	if (!mem) {
		mem = new NodeMapType();
	}

	NodeMapType::iterator ret = mem->find(realOffset);
	Node *retNode = nullptr;

	if (ret == mem->end()) {
		Node *newNode = new Node(Node::LAZY_FLATTEN, nullptr);
		mem->operator[](realOffset) = newNode;

		retNode = newNode;
	} else {
		retNode = ret->second;
	}

	return retNode;
}

Node* Node::getNode() {
	return getNode(0);
}

void Node::insertNode(Node *node) {
	insertNode(0, node);
}

void Node::insertNode(int64_t offset, Node *node) {

	if (!node)
		llvm_unreachable("insert null node\n");

	int64_t realOffset = startOffset + offset;

	if (realOffset < 0)
		llvm_unreachable("wrong offset");

	if (!mem)
		mem = new NodeMapType ();

	auto it = mem->find(realOffset);
	if (it != mem->end()) {
		it->second = node;
	} else {
		pair<int64_t, Node*> p (realOffset, node);
		mem->insert(p);
	}
}

#if 0
void Node::dump() {
	if (!mem) {
		errs() << "mem is nullptr\n";
		//llvm_unreachable("mem is nullptr");
	}

	auto iter = mem->find(0);
	if (iter != mem->end() && (*mem)[0] == nullptr) {
		errs() << "error here\n";
		llvm_unreachable("here");
	}
}
#else
void Node::dump() {
	std::unordered_set<NodeMapType *> visited;
	dump(visited, 0);
	visited.clear();
}

void Node::dump(std::unordered_set<NodeMapType *> &visited, unsigned indent) 
{
	ofs->indent(indent) << ">>>>>>>node@" << this << "\n";
	if (const Function * F = dyn_cast_or_null<Function>(value))
		ofs->indent(indent) << "v: @" << F->getName() << "\n";
	else if (value)
		ofs->indent(indent) << "v: " << *value << '\n';
	else
		ofs->indent(indent) << "v: null\n";

	ofs->indent(indent) << "startOff: " << startOffset << '\n';

	ofs->indent(indent) << "type: ";
	switch (type) {
		case GLOBAL:
			(*ofs) << "GLOBAL"; break;
		case STACK:
			(*ofs) << "STACK"; break;
		case HEAP:
			(*ofs) << "HEAP"; break;
		case SPECIAL_STACK:
			(*ofs) << "SPECIAL_STACK"; break;
		case UNDEFINED:
			(*ofs) << "UNDEFINED"; break;
		case LAZY_FLATTEN:
			(*ofs) << "LAZY_FLATTEN"; break;
		default:
			(*ofs) << "unknown type"; break;
	}
	(*ofs) << "\n";

	if (!mem) {
		ofs->indent(indent) << "mem is nullptr\n";
		//llvm_unreachable("here");
	} else {
		ofs->indent(indent) << "map size: " << mem->size() << "@" << mem << '\n';
		
		if (visited.find(this->mem) == visited.end()) {
			visited.insert(this->mem);

			NodeMapType::iterator it = mem->begin(), ie = mem->end();
			for (; it != ie; ++it) {
				//if (it->first != startOffset)
				//	continue;
				ofs->indent(indent) << "offset: " << it->first << " -> ";
				Node *subNode = it->second;
				if (subNode) {
					(*ofs) <<"node@" << subNode << '\n';
					subNode->dump(visited, indent + 1);
				} else
					llvm_unreachable("null node\n");
			}
		}
	}
	ofs->indent(indent) << "<<<<<<<"<< "\n";

	ofs->flush();
}
#endif
