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
	virtual ~CDB_OptimizeTree() override;

	void Store(IWriter* pWriter);
	bool Restore(IReader* pReader);

	virtual bool Build(Opcode::AABBTree* tree) override;
};