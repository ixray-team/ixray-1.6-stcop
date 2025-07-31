#include "stdafx.h"

#include "XRayModelPool.h"
#include "XRayFHierrarhyVisual.h"
#include "XRayFLOD.h"
#include "XRayFProgressive.h"
#include "XRayFSkinned.h"
#include "XRayFVisual.h"
#include "XRayKinematics.h"
#include "XRayKinematicsAnimated.h"
#include "XRayTreeVisual.h"
#include "../../xrEngine/IGame_Persistent.h"
#include "../../xrEngine/fmesh.h"

CDS0_ModelPool* GModelPool;
CDS0_RenderVisual* CDS0_ModelPool::Instance_Create(u32 type)
{
	CDS0_RenderVisual* V = NULL;

	// Check types
	switch (type) {
	case MT_NORMAL:				// our base visual
		V = new CDS0_FVisual;
		break;
	case MT_HIERRARHY:
		V = new CDS0_FHierrarhyVisual;
		break;
	case MT_PROGRESSIVE:		// dynamic-resolution visual
		V = new CDS0_FProgressive;
		break;
	case MT_SKELETON_ANIM:
		V = new CDS0_KinematicsAnimated;
		break;
	case MT_SKELETON_RIGID:
		V = new CDS0_Kinematics;
		break;
	case MT_SKELETON_GEOMDEF_PM:
		V = new CDS0_SkeletonX_PM;
		break;
	case MT_SKELETON_GEOMDEF_ST:
		V = new CDS0_SkeletonX_ST;
		break;
	case MT_PARTICLE_EFFECT:
		break;
	case MT_PARTICLE_GROUP:
		break;
#ifndef _EDITOR
	case MT_LOD:
		//R_ASSERT(0);
		V = new CDS0_FLOD;
		break;
	case MT_TREE_ST:
		V = new CDS0_TreeVisual_ST;
		break;
	case MT_TREE_PM:
		V = new CDS0_TreeVisual_PM;
		break;
#endif
	default:
		FATAL("Unknown visual type");
		break;
	}
	R_ASSERT(V);
	V->Type = type;
	return		V;
}

CDS0_RenderVisual* CDS0_ModelPool::Instance_Duplicate(CDS0_RenderVisual* V)
{
	R_ASSERT(V);
	CDS0_RenderVisual* N = Instance_Create(V->Type);
	N->Copy(V);
	N->Spawn();
	// inc ref counter
	for (xr_vector<ModelDef>::iterator I = Models.begin(); I != Models.end(); I++)
		if (I->model == V)
		{
			I->refs++;
			break;
		}
	return N;
}

CDS0_RenderVisual* CDS0_ModelPool::Instance_Load(const char* N, BOOL allow_register)
{
	CDS0_RenderVisual* V;
	string_path		name;

	string_path fn;
	// Add default ext if no ext at all
	if (!strext(N))
	{
		xr_strcpy(name, N);
		xr_strcat(name, ".ogf");
	}
	else
		strcpy_s(name, sizeof(name), N);

	// Load data from MESHES or LEVEL
	if (!FS.exist(N))
	{
		if (!FS.exist(fn, "$level$", name) && !FS.exist(fn, "$game_meshes$", name))
			return nullptr;
	}
	else strcpy_s(fn, N);

	// Actual loading
	IReader* data = FS.r_open(fn);
	ogf_header H;
	data->r_chunk_safe(OGF_HEADER, &H, sizeof(H));

	V = Instance_Create(H.type);
	V->Load(N, data, 0);
	FS.r_close(data);
	g_pGamePersistent->RegisterModel(V);

	// Registration
	if (allow_register) Instance_Register(N, V);

	return V;
}

CDS0_RenderVisual* CDS0_ModelPool::Instance_Load(LPCSTR name, IReader* data, BOOL allow_register)
{
	CDS0_RenderVisual* V;

	ogf_header			H;
	data->r_chunk_safe(OGF_HEADER, &H, sizeof(H));
	V = Instance_Create(H.type);
	V->Load(name, data, 0);

	// Registration
	if (allow_register) Instance_Register(name, V);
	return V;
}

void		CDS0_ModelPool::Instance_Register(LPCSTR N, CDS0_RenderVisual* V)
{
	// Registration
	ModelDef			M;
	M.name = N;
	M.model = V;
	Models.push_back(M);
}


void CDS0_ModelPool::Destroy()
{
	// Pool
	Pool.clear();

	// Registry
	while (!Registry.empty()) {
		REGISTRY_IT it = Registry.begin();
		CDS0_RenderVisual* V = (CDS0_RenderVisual*)it->first;
#ifdef _DEBUG
		Msg("ModelPool: Destroy object: '%s'", *V->getDebugName().c_str());
#endif
		DeleteInternal(V, TRUE);
	}

	// Base/Reference
	xr_vector<ModelDef>::iterator	I = Models.begin();
	xr_vector<ModelDef>::iterator	E = Models.end();
	for (; I != E; I++)
	{
		I->model->Release();
		xr_delete(I->model);
	}

	Models.clear();

	// cleanup motions container
	g_pMotionsContainer->clean(false);
}

CDS0_ModelPool::CDS0_ModelPool()
{
	bLogging = TRUE;
	bForceDiscard = FALSE;
	bAllowChildrenDuplicate = TRUE;
	g_pMotionsContainer = new motions_container;
}

CDS0_ModelPool::~CDS0_ModelPool()
{
	Destroy();
	xr_delete(g_pMotionsContainer);
}

CDS0_RenderVisual* CDS0_ModelPool::Instance_Find(LPCSTR N)
{
	CDS0_RenderVisual* Model = 0;
	xr_vector<ModelDef>::iterator	I;
	for (I = Models.begin(); I != Models.end(); I++)
	{
		if (I->name[0] && (0 == xr_strcmp(*I->name, N))) {
			Model = I->model;
			break;
		}
	}
	return Model;
}

CDS0_RenderVisual* CDS0_ModelPool::Create(const char* name, IReader* data)
{
#ifdef _EDITOR
	if (!name || !name[0])	return 0;
#endif
	string_path low_name;	VERIFY(xr_strlen(name) < sizeof(low_name));
	xr_strcpy(low_name, name);	xr_strlwr(low_name);
	if (strext(low_name))	*strext(low_name) = 0;
	//	Msg						("-CREATE %s",low_name);

		// 0. Search POOL
	POOL_IT	it = Pool.find(low_name);
	if (it != Pool.end())
	{
		// 1. Instance found
		CDS0_RenderVisual* Model = it->second;
		Model->Spawn();
		Pool.erase(it);
		return				Model;
	}
	else {
		// 1. Search for already loaded model (reference, base model)
		CDS0_RenderVisual* Base = Instance_Find(low_name);

		if (0 == Base) {
			// 2. If not found
			bAllowChildrenDuplicate = FALSE;
			if (data)		Base = Instance_Load(low_name, data, TRUE);
			else			Base = Instance_Load(low_name, TRUE);
			bAllowChildrenDuplicate = TRUE;
#ifdef _EDITOR
			if (!Base)		return 0;
#endif
		}
		// 3. If found - return (cloned) reference
		CDS0_RenderVisual* Model = Instance_Duplicate(Base);
		Registry.insert(std::make_pair(Model, low_name));
		return				Model;
	}
}

CDS0_RenderVisual* CDS0_ModelPool::CreateChild(LPCSTR name, IReader* data)
{
	string256 low_name;		VERIFY(xr_strlen(name) < 256);
	xr_strcpy(low_name, name);	xr_strlwr(low_name);
	if (strext(low_name))	*strext(low_name) = 0;

	// 1. Search for already loaded model
	CDS0_RenderVisual* Base = Instance_Find(low_name);
	//.	if (0==Base) Base	 	= Instance_Load(name,data,FALSE);
	if (0 == Base)
	{
		if (data)		Base = Instance_Load(low_name, data, FALSE);
		else			Base = Instance_Load(low_name, FALSE);
	}

	CDS0_RenderVisual* Model = bAllowChildrenDuplicate ? Instance_Duplicate(Base) : Base;
	return					Model;
}

extern ENGINE_API xr_atomic_bool g_bRendering;
void CDS0_ModelPool::DeleteInternal(CDS0_RenderVisual*& V, BOOL bDiscard)
{
	VERIFY(!g_bRendering);
	if (!V)
		return;

	V->Depart();
	if (bDiscard || bForceDiscard)
	{
		Discard(V, TRUE);
	}
	else 
	{
		//
		REGISTRY_IT	it = Registry.find(V);
		if (it != Registry.end())
		{
			// Registry entry found - move it to pool
			Pool.insert(std::make_pair(it->second, V));
		}
		else {
			// Registry entry not-found - just special type of visual / particles / etc.
			xr_delete(V);
		}
	}
	V = NULL;
}

void CDS0_ModelPool::Delete(CDS0_RenderVisual*& V, BOOL bDiscard)
{
	if (NULL == V)
		return;

	if (g_bRendering) {
		VERIFY(!bDiscard);
		ModelsToDelete.push_back(V);
	}
	else {
		DeleteInternal(V, bDiscard);
	}
	V = NULL;
}

void CDS0_ModelPool::DeleteQueue()
{
	for (u32 it = 0; it < ModelsToDelete.size(); it++)
		DeleteInternal(ModelsToDelete[it]);
	ModelsToDelete.clear();
}

void CDS0_ModelPool::Discard(CDS0_RenderVisual*& V, BOOL b_complete)
{
	//
	REGISTRY_IT	it = Registry.find(V);
	if (it != Registry.end())
	{
		// Pool - OK

			// Base
		const shared_str& name = it->second;
		xr_vector<ModelDef>::iterator I = Models.begin();
		xr_vector<ModelDef>::iterator I_e = Models.end();

		for (; I != I_e; ++I)
		{
			if (I->name == name)
			{
				if (b_complete || strchr(*name, '#'))
				{
					VERIFY(I->refs > 0);
					I->refs--;
					if (0 == I->refs)
					{
						bForceDiscard = TRUE;
						I->model->Release();
						xr_delete(I->model);
						Models.erase(I);
						bForceDiscard = FALSE;
					}
					break;
				}
				else {
					if (I->refs > 0)
						I->refs--;
					break;
				}
			}
		}
		// Registry
		xr_delete(V);
		//.		xr_free			(name);
		Registry.erase(it);
	}
	else {
		// Registry entry not-found - just special type of visual / particles / etc.
		xr_delete(V);
	}
	V = NULL;
}

void CDS0_ModelPool::Prefetch()
{
	Logging(FALSE);
	// prefetch visuals
	string256 section; 
	xr_strcpy(section, "prefetch_visuals_");
	xr_strcat(section, g_pGamePersistent->m_game_params.m_game_type);
	CInifile::Sect& sect = pSettings->r_section(section);

	for (const CInifile::Item& it : sect.Data)
	{
		CDS0_RenderVisual* pVis = Create(it.first.c_str());
	}

	Logging(TRUE);
}

void CDS0_ModelPool::ClearPool(BOOL b_complete)
{
	POOL_IT	_I = Pool.begin();
	POOL_IT	_E = Pool.end();
	for (; _I != _E; _I++) {
		Discard(_I->second, b_complete);
	}
	Pool.clear();
}

void CDS0_ModelPool::Render()
{
#if 0
	for (auto [Model, _] : Registry)
	{
		if (IKinematics* IK = Model->dcast_PKinematics())
		{
			IK->CalculateBones(true);
		}
	}
#endif
}