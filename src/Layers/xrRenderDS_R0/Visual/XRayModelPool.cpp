#include "XRayModelPool.h"

#include "stdafx.h"
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

XRayModelPool* GModelPool;
XRayRenderVisual* XRayModelPool::Instance_Create(u32 type)
{
	XRayRenderVisual* V = NULL;

	// Check types
	switch (type) {
	case MT_NORMAL:				// our base visual
		V = new XRayFVisual;
		break;
	case MT_HIERRARHY:
		V = new XRayFHierrarhyVisual;
		break;
	case MT_PROGRESSIVE:		// dynamic-resolution visual
		V = new XRayFProgressive;
		break;
	case MT_SKELETON_ANIM:
		V = new XRayKinematicsAnimated;
		break;
	case MT_SKELETON_RIGID:
		V = new XRayKinematics;
		break;
	case MT_SKELETON_GEOMDEF_PM:
		V = new XRaySkeletonX_PM;
		break;
	case MT_SKELETON_GEOMDEF_ST:
		V = new XRaySkeletonX_ST;
		break;
	case MT_PARTICLE_EFFECT:
		break;
	case MT_PARTICLE_GROUP:
		break;
#ifndef _EDITOR
	case MT_LOD:
		//R_ASSERT(0);
		V = new XRayFLOD;
		break;
	case MT_TREE_ST:
		V = new XRayTreeVisual_ST;
		break;
	case MT_TREE_PM:
		V = new XRayTreeVisual_PM;
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

XRayRenderVisual* XRayModelPool::Instance_Duplicate(XRayRenderVisual* V)
{
	R_ASSERT(V);
	XRayRenderVisual* N = Instance_Create(V->Type);
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

XRayRenderVisual* XRayModelPool::Instance_Load(const char* N, BOOL allow_register)
{
	XRayRenderVisual* V;
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

XRayRenderVisual* XRayModelPool::Instance_Load(LPCSTR name, IReader* data, BOOL allow_register)
{
	XRayRenderVisual* V;

	ogf_header			H;
	data->r_chunk_safe(OGF_HEADER, &H, sizeof(H));
	V = Instance_Create(H.type);
	V->Load(name, data, 0);

	// Registration
	if (allow_register) Instance_Register(name, V);
	return V;
}

void		XRayModelPool::Instance_Register(LPCSTR N, XRayRenderVisual* V)
{
	// Registration
	ModelDef			M;
	M.name = N;
	M.model = V;
	Models.push_back(M);
}


void XRayModelPool::Destroy()
{
	// Pool
	Pool.clear();

	// Registry
	while (!Registry.empty()) {
		REGISTRY_IT it = Registry.begin();
		XRayRenderVisual* V = (XRayRenderVisual*)it->first;
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

XRayModelPool::XRayModelPool()
{
	bLogging = TRUE;
	bForceDiscard = FALSE;
	bAllowChildrenDuplicate = TRUE;
	g_pMotionsContainer = new motions_container;
}

XRayModelPool::~XRayModelPool()
{
	Destroy();
	xr_delete(g_pMotionsContainer);
}

XRayRenderVisual* XRayModelPool::Instance_Find(LPCSTR N)
{
	XRayRenderVisual* Model = 0;
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

XRayRenderVisual* XRayModelPool::Create(const char* name, IReader* data)
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
		XRayRenderVisual* Model = it->second;
		Model->Spawn();
		Pool.erase(it);
		return				Model;
	}
	else {
		// 1. Search for already loaded model (reference, base model)
		XRayRenderVisual* Base = Instance_Find(low_name);

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
		XRayRenderVisual* Model = Instance_Duplicate(Base);
		Registry.insert(std::make_pair(Model, low_name));
		return				Model;
	}
}

XRayRenderVisual* XRayModelPool::CreateChild(LPCSTR name, IReader* data)
{
	string256 low_name;		VERIFY(xr_strlen(name) < 256);
	xr_strcpy(low_name, name);	xr_strlwr(low_name);
	if (strext(low_name))	*strext(low_name) = 0;

	// 1. Search for already loaded model
	XRayRenderVisual* Base = Instance_Find(low_name);
	//.	if (0==Base) Base	 	= Instance_Load(name,data,FALSE);
	if (0 == Base)
	{
		if (data)		Base = Instance_Load(low_name, data, FALSE);
		else			Base = Instance_Load(low_name, FALSE);
	}

	XRayRenderVisual* Model = bAllowChildrenDuplicate ? Instance_Duplicate(Base) : Base;
	return					Model;
}

extern ENGINE_API xr_atomic_bool g_bRendering;
void XRayModelPool::DeleteInternal(XRayRenderVisual*& V, BOOL bDiscard)
{
	VERIFY(!g_bRendering);
	if (!V)					return;
	V->Depart();
	if (bDiscard || bForceDiscard) {
		Discard(V, TRUE);
	}
	else {
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

void	XRayModelPool::Delete(XRayRenderVisual*& V, BOOL bDiscard)
{
	if (NULL == V)				return;
	if (g_bRendering) {
		VERIFY(!bDiscard);
		ModelsToDelete.push_back(V);
	}
	else {
		DeleteInternal(V, bDiscard);
	}
	V = NULL;
}

void	XRayModelPool::DeleteQueue()
{
	for (u32 it = 0; it < ModelsToDelete.size(); it++)
		DeleteInternal(ModelsToDelete[it]);
	ModelsToDelete.clear();
}

void	XRayModelPool::Discard(XRayRenderVisual*& V, BOOL b_complete)
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

void XRayModelPool::Prefetch()
{
	Logging(FALSE);
	// prefetch visuals
	string256 section; 
	xr_strcpy(section, "prefetch_visuals_");
	xr_strcat(section, g_pGamePersistent->m_game_params.m_game_type);
	CInifile::Sect& sect = pSettings->r_section(section);

	for (const CInifile::Item& it : sect.Data)
	{
		XRayRenderVisual* pVis = Create(it.first.c_str());
	}

	Logging(TRUE);
}

void XRayModelPool::ClearPool(BOOL b_complete)
{
	POOL_IT	_I = Pool.begin();
	POOL_IT	_E = Pool.end();
	for (; _I != _E; _I++) {
		Discard(_I->second, b_complete);
	}
	Pool.clear();
}
void XRayModelPool::Render()
{
	for (auto [_, Obj] : Pool)
	{
		if (auto Kinematic = Obj->dcast_PKinematics())
		{
			Kinematic->CalculateBones(true);
		}
	}
}
/*
XRayRenderVisual* XRayModelPool::CreatePE(PS::CPEDef* source)
{
	PS::CParticleEffect* V = (PS::CParticleEffect*)Instance_Create(MT_PARTICLE_EFFECT);
	V->Compile(source);
	return V;
}

XRayRenderVisual* XRayModelPool::CreatePG(PS::CPGDef* source)
{
	PS::CParticleGroup* V = (PS::CParticleGroup*)Instance_Create(MT_PARTICLE_GROUP);
	V->Compile(source);
	return V;
}*/


#ifdef _EDITOR
IC bool	_IsBoxVisible(XRayRenderVisual* visual, const Fmatrix& transform)
{
	Fbox 		bb;
	bb.xform(visual->vis.box, transform);
	return 		::Render->occ_visible(bb);
}
IC bool	_IsValidShader(XRayRenderVisual* visual, u32 priority, bool strictB2F)
{
	if (visual->shader)
		return (priority == visual->shader->E[0]->flags.iPriority) && (strictB2F == visual->shader->E[0]->flags.bStrictB2F);
	return false;
}

void 	XRayModelPool::Render(XRayRenderVisual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD)
{
	// render visual
	xr_vector<XRayRenderVisual*>::iterator I, E;
	switch (m_pVisual->Type) {
	case MT_SKELETON_ANIM:
	case MT_SKELETON_RIGID: {
		if (_IsBoxVisible(m_pVisual, mTransform)) {
			CKinematics* pV = dynamic_cast<CKinematics*>(m_pVisual); VERIFY(pV);
			if (fis_zero(m_fLOD, EPS) && pV->m_lod) {
				if (_IsValidShader(pV->m_lod, priority, strictB2F)) {
					RCache.set_Shader(pV->m_lod->shader ? pV->m_lod->shader : EDevice->m_WireShader);
					RCache.set_xform_world(mTransform);
					pV->m_lod->Render(1.f);
				}
			}
			else {
				I = pV->children.begin();
				E = pV->children.end();
				for (; I != E; I++) {
					if (_IsValidShader(*I, priority, strictB2F)) {
						RCache.set_Shader((*I)->shader ? (*I)->shader : EDevice->m_WireShader);
						RCache.set_xform_world(mTransform);
						(*I)->Render(m_fLOD);
					}
				}
			}
		}
	}break;
	case MT_HIERRARHY: {
		if (_IsBoxVisible(m_pVisual, mTransform)) {
			FHierrarhyVisual* pV = dynamic_cast<FHierrarhyVisual*>(m_pVisual); VERIFY(pV);
			I = pV->children.begin();
			E = pV->children.end();
			for (; I != E; I++) {
				if (_IsValidShader(*I, priority, strictB2F)) {
					RCache.set_Shader((*I)->shader ? (*I)->shader : EDevice->m_WireShader);
					RCache.set_xform_world(mTransform);
					(*I)->Render(m_fLOD);
				}
			}
		}
	}break;
	case MT_PARTICLE_GROUP: {
		PS::CParticleGroup* pG = dynamic_cast<PS::CParticleGroup*>(m_pVisual); VERIFY(pG);
		//		if (_IsBoxVisible(m_pVisual,mTransform))
		{
			RCache.set_xform_world(mTransform);
			for (PS::CParticleGroup::SItemVecIt i_it = pG->items.begin(); i_it != pG->items.end(); i_it++) {
				xr_vector<XRayRenderVisual*>	visuals;
				i_it->GetVisuals(visuals);
				for (xr_vector<XRayRenderVisual*>::iterator it = visuals.begin(); it != visuals.end(); it++)
					Render(*it, Fidentity, priority, strictB2F, m_fLOD);
			}
		}
	}break;
	case MT_PARTICLE_EFFECT: {
		//		if (_IsBoxVisible(m_pVisual,mTransform))
		{
			if (_IsValidShader(m_pVisual, priority, strictB2F)) {
				RCache.set_Shader(m_pVisual->shader ? m_pVisual->shader : EDevice->m_WireShader);
				RCache.set_xform_world(mTransform);
				m_pVisual->Render(m_fLOD);
			}
		}
	}break;
	default:
		if (_IsBoxVisible(m_pVisual, mTransform)) {
			if (_IsValidShader(m_pVisual, priority, strictB2F)) {
				RCache.set_Shader(m_pVisual->shader ? m_pVisual->shader : EDevice->m_WireShader);
				RCache.set_xform_world(mTransform);
				m_pVisual->Render(m_fLOD);
			}
		}
		break;
	}
}

void 	XRayModelPool::RenderSingle(XRayRenderVisual* m_pVisual, const Fmatrix& mTransform, float m_fLOD)
{
	for (int p = 0; p < 4; p++) {
		Render(m_pVisual, mTransform, p, false, m_fLOD);
		Render(m_pVisual, mTransform, p, true, m_fLOD);
	}
}
void XRayModelPool::OnDeviceDestroy()
{
	Destroy();
}
#endif
