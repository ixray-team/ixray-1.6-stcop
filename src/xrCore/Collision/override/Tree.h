//////////////////////////////////////////////////////////
// Desc   : Collision Detection OptTree + Cache System
// Author : ForserX
//////////////////////////////////////////////////////////
#pragma once
#include <Opcode.h>

class CDB_OptimizeTree : public Opcode::AABBNoLeafTree
{
public:
	CDB_OptimizeTree();
	~CDB_OptimizeTree();

	void Store(IWriter* pWriter);
	bool Restore(IReader* pReader);

	bool Build(Opcode::AABBTree* tree);
};