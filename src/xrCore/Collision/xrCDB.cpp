#include "stdafx.h"
#include "xrCDB.h"
#include "override/Model.h"
#include "API/xrAPI.h"
namespace Opcode 
{
#	include <OPC_TreeBuilders.h>
#	include <OPC_Model.h>
}

using namespace CDB;
using namespace Opcode;

XRCORE_API IReader* CDB::GetModelCache(string_path LevelName, u32 crc)
{
	IReader* pReaderCache = nullptr;

	if (FS.exist("$app_data_root$", LevelName))
	{
		pReaderCache = FS.r_open("$app_data_root$", LevelName);

		if (pReaderCache->length() <= 4 || pReaderCache->r_u32() != crc)
		{
			FS.r_close(pReaderCache);
		}
	}

	return pReaderCache;
}

IReader* CDB::GetModelCache(const xr_stack_string_path& LevelName, u32 crc)
{
	IReader* pReaderCache = nullptr;

	if (FS.exist("$app_data_root$", LevelName.c_str()))
	{
		pReaderCache = FS.r_open("$app_data_root$", LevelName.c_str());

		if (pReaderCache->length() <= 4 || pReaderCache->r_u32() != crc)
		{
			FS.r_close(pReaderCache);
		}
	}

	return pReaderCache;
}

CDB::MODEL::~MODEL()
{
	if (S_READY != status.load())
		load_task.wait();

	xr_delete(tree);
}

void MODEL::build(Fvector* V, size_t Vcnt, TRI* T, size_t Tcnt, build_callback* bc, void* bcp, void* pRW, bool RWMode, bool UseDelay)
{
	R_ASSERT((Vcnt >= 4) && (Tcnt >= 2));

	if (status.load() != S_INIT)
		return;

	status.store(S_BUILD);

	auto LoaderLamda = [=]()
	{
		PROF_START_THREAD("build cform");

		// verts
		if (verts.empty())
		{
			verts.resize(Vcnt);
			CopyMemory(verts.data(), V, Vcnt * sizeof(Fvector));
		}

		// tris
		if (tris.empty())
		{
			tris.resize(Tcnt);
			CopyMemory(tris.data(), T, Tcnt * sizeof(TRI));
		}

		// callback
		if (bc)
			bc(verts.data(), Vcnt, tris.data(), Tcnt, bcp);

		tree = new CDB_Model();

		if (pRW != nullptr && RWMode)
		{
			if (tree->Restore((IReader*)pRW))
			{
				Msg("* Collision DB cache found...");
				status.store(S_READY);
				PROF_STOP_THREAD();
				return;
			}
			else
				Msg("* Collision DB cache missing, rebuilding...");
		}

		// Build a non quantized no-leaf tree
		OPCODECREATE OPCC;

		OPCC.mIMesh = new MeshInterface();
		OPCC.mIMesh->SetNbTriangles(tris.size());
		OPCC.mIMesh->SetNbVertices(verts.size());
		OPCC.mIMesh->SetPointers((IceMaths::IndexedTriangle*)tris.data(), (IceMaths::Point*)verts.data());
		OPCC.mSettings.mRules = SplittingRules::SPLIT_SPLATTER_POINTS | SplittingRules::SPLIT_GEOM_CENTER;
		OPCC.mNoLeaf = true;
		OPCC.mQuantized = false;

		if (!tree->Build(OPCC))
		{
			status.store(S_READY);
			verts.clear();
			tris.clear();
			xr_delete(tree);
#ifdef DEBUG
			Msg("! Collision build failed");
#endif
			PROF_STOP_THREAD();
			return;
		}

		// Write cache
		if (!RWMode && pRW)
		{
			tree->Store((IWriter*)pRW);
			FS.w_close((IWriter*&)pRW);
		}
#ifdef DEBUG
		Msg("+ Collision build succeeded");
#endif
		status.store(S_READY);
		PROF_STOP_THREAD();
	};

	if (UseDelay)
	{
		load_task.run(LoaderLamda);
	}
	else
	{
		LoaderLamda();
	}
}

u32 MODEL::memory()
{
	if (S_READY != status.load())
	{
		Msg("! xrCDB: model still isn't ready");
		return 0;
	}
	u32 V = verts.size() * sizeof(Fvector);
	u32 T = tris.size() * sizeof(TRI);
	return tree->GetUsedBytes()+V+T+sizeof(*this)+sizeof(*tree);
}
