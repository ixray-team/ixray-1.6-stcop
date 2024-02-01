// xrCDB.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#pragma hdrstop

#include "xrCDB.h"
#include "override/Model.h"

namespace Opcode 
{
#	include <OPC_TreeBuilders.h>
#	include <OPC_Model.h>
}

using namespace CDB;
using namespace Opcode;

BOOL APIENTRY DllMain( HANDLE hModule, 
					  u32  ul_reason_for_call, 
					  LPVOID lpReserved
					  )
{
    switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
    }
    return TRUE;
}

XRCDB_API IReader* CDB::GetModelCache(string_path LevelName, u32 crc)
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

// Model building
MODEL::MODEL	()
#ifdef PROFILE_CRITICAL_SECTIONS
	:cs(MUTEX_PROFILE_ID(MODEL))
#endif // PROFILE_CRITICAL_SECTIONS
{
	tree		= 0;
	tris		= 0;
	tris_count	= 0;
	verts		= 0;
	verts_count	= 0;
	status		= S_INIT;
}
MODEL::~MODEL()
{
	syncronize	();		// maybe model still in building
	status		= S_INIT;
	CDELETE		(tree);
	CFREE		(tris);		tris_count = 0;
	CFREE		(verts);	verts_count= 0;
}

void MODEL::build(Fvector* V, int Vcnt, TRI* T, int Tcnt, build_callback* bc, void* bcp, void* pRW, bool RWMode)
{
	R_ASSERT(S_INIT == status);
	R_ASSERT((Vcnt >= 4) && (Tcnt >= 2));

	build_internal(V, Vcnt, T, Tcnt, bc, bcp, pRW, RWMode);
	status = S_READY;
}

void MODEL::build_internal(Fvector* V, int Vcnt, TRI* T, int Tcnt, build_callback* bc, void* bcp, void* pRW, bool RWMode)
{
	// verts
	verts_count = Vcnt;
	verts = CALLOC(Fvector, verts_count);
	CopyMemory(verts, V, verts_count * sizeof(Fvector));

	// tris
	tris_count = Tcnt;
	tris = CALLOC(TRI, tris_count);
	CopyMemory(tris, T, tris_count * sizeof(TRI));

	// callback
	if (bc)		
		bc(verts, Vcnt, tris, Tcnt, bcp);

	if (pRW != nullptr && RWMode)
	{
		IReader* pReader = (IReader*)(pRW);
		tree = new CDB_Model();

		if (tree->Restore(pReader))
		{
			Msg("* Level collision DB cache found...");
			return;
		}
		else
		{
			xr_delete(tree);
			Msg("* Level collision DB cache missing, rebuilding...");
		}
	}

	CreateNewTree(RWMode ? nullptr : (IWriter*)pRW);
}


void CDB::MODEL::CreateNewTree(IWriter* pCache)
{
	// Release data pointers
	status = S_BUILD;

	// Allocate temporary "OPCODE" tris + convert tris to 'pointer' form
	u32* temp_tris = CALLOC(u32, tris_count * 3);
	if (0 == temp_tris) {
		CFREE(verts);
		CFREE(tris);
		return;
	}
	u32* temp_ptr = temp_tris;
	for (int i = 0; i < tris_count; i++)
	{
		*temp_ptr++ = tris[i].verts[0];
		*temp_ptr++ = tris[i].verts[1];
		*temp_ptr++ = tris[i].verts[2];
	}

	// Build a non quantized no-leaf tree
	OPCODECREATE OPCC;

	OPCC.mIMesh = new MeshInterface();
	OPCC.mIMesh->SetNbTriangles(tris_count);
	OPCC.mIMesh->SetNbVertices(verts_count);
	OPCC.mIMesh->SetPointers((IceMaths::IndexedTriangle*)temp_tris, (IceMaths::Point*)verts);
	OPCC.mSettings.mRules = SplittingRules::SPLIT_SPLATTER_POINTS | SplittingRules::SPLIT_GEOM_CENTER;
	OPCC.mNoLeaf = true;
	OPCC.mQuantized = false;

	tree = CNEW(CDB_Model) ();
	if (!tree->Build(OPCC))
	{
		CFREE(verts);
		CFREE(tris);
		CFREE(temp_tris);
		return;
	};

	// Write cache
	if (pCache)
	{
		IWriter* pWritter = (IWriter*)(pCache);
		tree->Store(pWritter);
		FS.w_close(pWritter);
	}

	// Free temporary tris
	CFREE(temp_tris);
}

u32 MODEL::memory	()
{
	if (S_BUILD==status)	{ Msg	("! xrCDB: model still isn't ready"); return 0; }
	u32 V					= verts_count*sizeof(Fvector);
	u32 T					= tris_count *sizeof(TRI);
	return tree->GetUsedBytes()+V+T+sizeof(*this)+sizeof(*tree);
}

// This is the constructor of a class that has been exported.
// see xrCDB.h for the class definition
COLLIDER::COLLIDER()
{ 
	ray_mode		= 0;
	box_mode		= 0;
	frustum_mode	= 0;
}

COLLIDER::~COLLIDER()
{
	r_free			();
}

RESULT& COLLIDER::r_add	()
{
	rd.push_back		(RESULT());
	return rd.back		();
}

void COLLIDER::r_free	()
{
	rd.clear();
}
