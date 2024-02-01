#include "stdafx.h"
#include "Model.h"
#include <OPC_TreeBuilders.h>
#include <Opcode.h>

CDB_Model::CDB_Model()
{
	pTree = new CDB_OptimizeTree();
}

CDB_Model::~CDB_Model()
{
	Release();
}

void CDB_Model::Store(IWriter* writer)
{
	writer->w_u64(mModelCode);
	pTree->Store(writer);
}

bool CDB_Model::Restore(IReader* reader)
{
	if (reader->elapsed() < sizeof(u64))
	{
		Msg("* Level Collision DB cache file missing model code!");
		return false;
	}
	mModelCode = reader->r_u64();

	return pTree->Restore(reader);
}

bool CDB_Model::Build(const Opcode::OPCODECREATE& create)
{
	if (!create.mIMesh || !create.mIMesh->IsValid())	return false;

	if (create.mSettings.mLimit != 1)
	{
		Msg("OPCODE WARNING: supports complete trees only! Use mLimit = 1. Current mLimit = %d", create.mSettings.mLimit);
		return false;
	}

	u64 NbDegenerate = create.mIMesh->CheckTopology();

	if (NbDegenerate)
		Msg("OPCODE WARNING: found %d degenerate faces in model! Collision might report wrong results!\n", NbDegenerate);

	ReleaseBase();
	SetMeshInterface(create.mIMesh);

	u64 NbTris = create.mIMesh->GetNbTriangles();
	bool Status = false;

	if (NbTris == 1)
	{
		mModelCode |= ModelFlag::OPC_SINGLE_NODE;
		Status = true;
		goto FreeAndExit;
	}

	mSource = new Opcode::AABBTree();
	{
		Opcode::AABBTreeOfTrianglesBuilder TB;
		TB.mIMesh = create.mIMesh;
		TB.mSettings = create.mSettings;
		TB.mNbPrimitives = NbTris;
		if (!mSource->Build(&TB))	goto FreeAndExit;
	}

	if (!CreateTree(create.mNoLeaf, create.mQuantized))	goto FreeAndExit;
	if (!pTree->Build(mSource))	goto FreeAndExit;

	// Finally ok...
	Status = true;

FreeAndExit:
	if (!create.mKeepOriginal)
		xr_delete(mSource);

	return Status;
}

void CDB_Model::Release()
{
	xr_delete(pTree);
}