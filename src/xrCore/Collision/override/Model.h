//////////////////////////////////////////////////////////
// Desc   : Collision Detection Model + Cache System
// Author : ForserX
// Update : 20.04.2020 - Support for Hybrid Trees System 
//////////////////////////////////////////////////////////
#pragma once
#include "stdafx.h"
#include "Tree.h"

class CDB_Model : public Opcode::Model
{
public:
	CDB_Model();
	virtual ~CDB_Model();

	void Store(IWriter* pWriter);
	bool Restore(IReader* pReader);

	// Overload for using CDB_OptimizeTree into Build Model 
	bool Build(const Opcode::OPCODECREATE& create);
	virtual void Release() override;

	IC  CDB_OptimizeTree* GetTree() { return pTree; }

protected:
	CDB_OptimizeTree* pTree;

	enum ModelFlag
	{
		OPC_QUANTIZED = (1 << 0),	//!< Compressed/uncompressed tree
		OPC_NO_LEAF = (1 << 1),	//!< Leaf/NoLeaf tree
		OPC_SINGLE_NODE = (1 << 2)	//!< Special case for 1-node models
	};
};