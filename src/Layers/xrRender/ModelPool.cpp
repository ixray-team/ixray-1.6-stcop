#include "stdafx.h"


#include "ModelPool.h"

#include "../../xrEngine/FmeshRender.h"
#include "../../xrEngine/IGame_Persistent.h"
#ifndef _EDITOR
#	include "FLOD.h"
#	include "FTreeVisual.h"
#endif

#include "FVisual.h"
#include "FProgressive.h"
#include "ParticleEffect.h"
#include "ParticleGroup.h"
#include "SkeletonX.h"
#include "FHierrarhyVisual.h"
#include "FTreeVisual_Prototype.h"
#include "SkeletonAnimated.h"

dxRender_Visual* CModelPool::Instance_Create(u32 type)
{
	dxRender_Visual *V = nullptr;

	// Check types
	switch (type) {
	case MT_NORMAL:				// our base visual
		V	= new Fvisual				();
		break;
	case MT_HIERRARHY:
		V	= new FHierrarhyVisual		();
		break;
	case MT_PROGRESSIVE:		// dynamic-resolution visual
		V	= new FProgressive			();
		break;
	case MT_SKELETON_ANIM:
		V	= new CKinematicsAnimated	();
		break;
	case MT_SKELETON_RIGID:
		V	= new CKinematics			();
		break;
	case MT_SKELETON_GEOMDEF_PM:
		V	= new CSkeletonX(true);
		break;
	case MT_SKELETON_GEOMDEF_ST:
		V	= new CSkeletonX(false);
		break;
	case MT_PARTICLE_EFFECT:
		V	= new PS::CParticleEffect	();
		break;
	case MT_PARTICLE_GROUP:
		V	= new PS::CParticleGroup	();
		break;
#ifndef _EDITOR
	case MT_LOD:
	{
	}
	case MT_MESH_LODS:
	{
		V = new FMUMeshLODs();	
		break;
	}
	case MT_LOD0:
	{
		auto New = new FMUMeshLOD();
		New->LODLevel = 0;
		V = New;
		break;
	}
	case MT_LOD1:
	{
		auto New = new FMUMeshLOD();
		New->LODLevel = 1;
		V = New;
		break;
	}
	case MT_LOD2:
	{
		auto New = new FMUMeshLOD();
		New->LODLevel = 2;
		V = New;
		break;
	}
	case MT_LOD3:
	{
		auto New = new FMUMeshLOD();
		New->LODLevel = 3;
		V = New;
		break;
	}
	case MT_LOD4:
	{
		auto New = new FMUMeshLOD();
		New->LODLevel = 4;
		V = New;
		break;
	}
	case MT_TREE_ST:
		V	= new FTreeVisual_ST		();
		break;
	case MT_TREE_PM:
		V	= new FTreeVisual_PM		();
		break;
	case MT_TREE_PROTOTYPE:
		V	= new FTreeVisual_Prototype		();
		break;
#endif
	default:
		FATAL	("Unknown visual type");
		break;
	}
	R_ASSERT	(V);
	V->Type		= type;
	return		V;
}

dxRender_Visual*	CModelPool::Instance_Duplicate	(dxRender_Visual* V)
{
	R_ASSERT(V);
	dxRender_Visual* N		= Instance_Create(V->Type);
	N->Copy			(V);
	N->Spawn		();
    // inc ref counter
	for (xr_vector<ModelDef>::iterator I=Models.begin(); I!=Models.end(); I++) 
		if (I->model==V)
		{ 
			I->refs++; 
			break;
		}
	return N;
}

dxRender_Visual*	CModelPool::Instance_Load		(const char* InputName, bool allow_register)
{
	PROF_EVENT(__FUNCTION__);
	dxRender_Visual	*V;
	string_path		fn;
	string_path		name;

	// Add default ext if no ext at all
	string_path N = {};
	xr_strcpy(N, Platform::ValidPath(InputName));

	if (nullptr==strext(N))
	{
		xr_strconcat(name, N, ".ogf");
	}
	else
	{	
		xr_strcpy(name, sizeof(name), N);
	}

	// Load data from MESHES or LEVEL
	if (!FS.exist(N))
	{
		if (!FS.exist(fn, "$level$", name))
		{
			if (!FS.exist(fn, _game_meshes_, name))
			{
#ifdef _EDITOR
				Msg("! Can't find model file '%s'.",name);
#else            
				Debug.fatal(DEBUG_INFO,"Can't find model file '%s'.",name);
#endif
				return nullptr;
			}
		}
	}
	else 
	{
		xr_strcpy(fn,N);
	}
	
	// Actual loading
	IReader* data = FS.r_open(fn);
	ogf_header H;
	data->r_chunk_safe(OGF_HEADER,&H,sizeof(H));
	V = Instance_Create(H.type);
	V->Load(N,data,0);
	FS.r_close(data);
	g_pGamePersistent->RegisterModel(V);

	// Registration
	if (allow_register)
	{
		Instance_Register(N,V);
	}
	
	return V;
}

dxRender_Visual* CModelPool::Instance_Load(const char* name, IReader* data, bool allow_register)
{
	PROF_EVENT(__FUNCTION__);
	dxRender_Visual	*V;
	
	ogf_header			H;
	data->r_chunk_safe	(OGF_HEADER,&H,sizeof(H));
	V = Instance_Create (H.type);
	V->Load				(name,data,0);

	// Registration
	if (allow_register) Instance_Register(name,V);
	return V;
}

void CModelPool::Instance_Register(const char* N, dxRender_Visual* V)
{
	// Registration
	ModelDef			M;
	M.name				= N;
	M.model				= V;
	Models.push_back	(M);
}


void CModelPool::Destroy()
{
	// Pool
	Pool.clear();

	// Registry
	while(!Registry.empty())
	{
		REGISTRY_IT it	= Registry.begin();
		if (it == Registry.end())
			break;

		dxRender_Visual* V=(dxRender_Visual*)it->first;
		DeleteInternal	(V,true);
	}

	// Base/Reference
	xr_vector<ModelDef>::iterator	I = Models.begin();
	xr_vector<ModelDef>::iterator	E = Models.end();
	for (; I!=E; I++)
	{
		I->model->Release();
		xr_delete(I->model);
	}
	
	Models.clear();

	// cleanup motions container
	g_pMotionsContainer->clean(false);
}

CModelPool::CModelPool()
{
    bForceDiscard 			= false;
    bAllowChildrenDuplicate	= true; 
	g_pMotionsContainer		= new motions_container();
}

CModelPool::~CModelPool()
{
	Destroy					();
	xr_delete				(g_pMotionsContainer);
}

dxRender_Visual* CModelPool::Instance_Find(const char* N)
{
	dxRender_Visual*				Model=nullptr;
	xr_vector<ModelDef>::iterator	I;
	for (I=Models.begin(); I!=Models.end(); I++)
	{
		if (I->name[0]&&(0==xr_strcmp(*I->name,N))) {
			Model = I->model;
			break;
		}
	}
	return Model;
}

dxRender_Visual* CModelPool::Create(const char* name, IReader* data)
{
#ifdef _EDITOR
	if (!name||!name[0])	return nullptr;
#endif
	string_path low_name;	VERIFY	(xr_strlen(name)<sizeof(low_name));
	xr_strcpy(low_name,name);	_strlwr	(low_name);
	if (strext(low_name))	*strext	(low_name)=0;
//	Msg						("-CREATE %s",low_name);

	// 0. Search POOL
	POOL_IT	it			=	Pool.find	(low_name);
	if (it!=Pool.end())
	{
		// 1. Instance found
        dxRender_Visual*		Model	= it->second;
		Model->Spawn		();
		Pool.erase			(it);
		return				Model;
	} else {
		// 1. Search for already loaded model (reference, base model)
		dxRender_Visual* Base		= Instance_Find		(low_name);

		if (nullptr==Base){
			// 2. If not found
			bAllowChildrenDuplicate	= false;
			if (data)		Base = Instance_Load(low_name,data,true);
            else			Base = Instance_Load(low_name,true);
			bAllowChildrenDuplicate	= true;
#ifdef _EDITOR
			if (!Base)		return nullptr;
#endif
		}
        // 3. If found - return (cloned) reference
        dxRender_Visual*		Model	= Instance_Duplicate(Base);
        Registry.insert		( std::make_pair(Model,low_name) );
        return				Model;
	}
}

dxRender_Visual* CModelPool::CreateChild(const char* name, IReader* data)
{
	string256 low_name;		VERIFY	(xr_strlen(name)<256);
	xr_strcpy(low_name,name);	_strlwr	(low_name);
	if (strext(low_name))	*strext	(low_name) = 0;

	// 1. Search for already loaded model
	dxRender_Visual* Base	= Instance_Find(low_name);
//.	if (0==Base) Base	 	= Instance_Load(name,data,false);
	if(nullptr==Base)
	{
		if (data)		Base = Instance_Load	(low_name,data,false);
		else			Base = Instance_Load	(low_name,false);
	}

    dxRender_Visual* Model	= bAllowChildrenDuplicate?Instance_Duplicate(Base):Base;
    return					Model;
}

dxRender_Visual* CModelPool::GetPrototype(const char* name)
{
	string_path low_name;	
	VERIFY(xr_strlen(name)<sizeof(low_name));
	xr_strcpy(low_name,name);	
	_strlwr	(low_name);
	if (strext(low_name))
	{
		*strext(low_name)=0;
	}
	
	dxRender_Visual* Base = Instance_Find(low_name);

	if (!Base)
	{
		bAllowChildrenDuplicate	= false;
		Base = Instance_Load(low_name,true);
		bAllowChildrenDuplicate	= true;
	}
	return Base;
}

extern  xr_atomic_bool ENGINE_API g_bRendering; 
void	CModelPool::DeleteInternal	(dxRender_Visual* &V, bool bDiscard)
{
	VERIFY					(!g_bRendering);
    if (!V)					return;
	V->Depart				();
	if (bDiscard||bForceDiscard){
    	Discard	(V, true); 
	}else{
		//
		REGISTRY_IT	it		= Registry.find	(V);
		if (it!=Registry.end())
		{
			// Registry entry found - move it to pool
			Pool.insert			(std::make_pair(it->second,V));
		} else {
			// Registry entry not-found - just special type of visual / particles / etc.
			xr_delete			(V);
		}
	}
	V	=	nullptr;
}

void CModelPool::DeleteDeffered(dxRender_Visual* &V)
{
	if (nullptr==V)
		return;

	xrCriticalSectionGuard guard(&deffered_del_lock);

	ModelsToDeleteDeffer.insert(V);
	V = nullptr;
}

void CModelPool::Delete(dxRender_Visual* &V, bool bDiscard)
{
	if (nullptr==V)
		return;

	if (g_bRendering)
	{
		VERIFY(!bDiscard);
		ModelsToDelete.push_back(V);
	} 
	else
	{
		DeleteInternal(V,bDiscard);
	}

	V =	nullptr;
}

void CModelPool::DeleteQueue()
{
	for (u32 it = 0; it < ModelsToDelete.size(); it++)
		DeleteInternal(ModelsToDelete[it]);
	ModelsToDelete.clear();
}

void CModelPool::DeleteQueuedDeffer()
{
	xrCriticalSectionGuard guard(&deffered_del_lock);

	for (dxRender_Visual* Vis : ModelsToDeleteDeffer)
		DeleteInternal(Vis);

	ModelsToDeleteDeffer.clear();
}

void CModelPool::Discard(dxRender_Visual*& V, bool b_complete)
{
	//
	REGISTRY_IT	it = Registry.find(V);
	if (it != Registry.end())
	{
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
						bForceDiscard = true;
						I->model->Release();
						xr_delete(I->model);
						Models.erase(I);
						bForceDiscard = false;
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
		Registry.erase(it);
	}
	else
	{
		// Registry entry not-found - just special type of visual / particles / etc.
		xr_delete(V);
	}

	V = nullptr;
}

void CModelPool::Prefetch()
{
	// prefetch visuals
	xr_string section = "prefetch_visuals_";
	section += g_pGamePersistent->m_game_params.m_game_type;

	// Workaround for SoC sections
	if (!pSettings->section_exist(section.c_str()))
	{
		if (section.compare("prefetch_visuals_dm"))
			section = "prefetch_visuals_deathmatch";
		if (section.compare("prefetch_visuals_tdm"))
			section = "prefetch_visuals_teamdeathmatch";
		if (section.compare("prefetch_visuals_ah"))
			section = "prefetch_visuals_artefacthunt";
	}

	CInifile::Sect& sect = pSettings->r_section(section.c_str());
	for (CInifile::SectCIt I = sect.Data.begin(); I != sect.Data.end(); I++)
	{
		const CInifile::Item& item = *I;
		dxRender_Visual* V = Create(item.first.c_str());
		Delete(V, false);
	}
}

void CModelPool::ClearPool( bool b_complete)
{
	POOL_IT	_I			=	Pool.begin();
	POOL_IT	_E			=	Pool.end();
	for (;_I!=_E;_I++)	{
		Discard	(_I->second, b_complete)	;
	}
	Pool.clear			();
}

dxRender_Visual* CModelPool::CreatePE	(PS::CPEDef* source)
{
	PS::CParticleEffect* V	= (PS::CParticleEffect*)Instance_Create(MT_PARTICLE_EFFECT);
	V->Compile		(source);
	return V;
}

dxRender_Visual* CModelPool::CreatePG	(PS::CPGDef* source)
{
	PS::CParticleGroup* V	= (PS::CParticleGroup*)Instance_Create(MT_PARTICLE_GROUP);
	V->Compile		(source);
	return V;
}

#ifdef _EDITOR
IC bool	_IsBoxVisible(dxRender_Visual* visual, const Fmatrix& transform)
{
    Fbox 		bb; 
    bb.xform	(visual->vis.box,transform);
    return 		::Render->occ_visible(bb);
}
IC bool	_IsValidShader(dxRender_Visual* visual, u32 priority, bool strictB2F)
{
	if (visual->shader && visual->shader->E[0])
        return (priority==visual->shader->E[0]->flags.iPriority)&&(strictB2F==visual->shader->E[0]->flags.bStrictB2F);
    return false;
}

void 	CModelPool::Render(dxRender_Visual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD)
{
    // render visual
    xr_vector<dxRender_Visual*>::iterator I,E;
    switch (m_pVisual->Type){
    case MT_SKELETON_ANIM:
    case MT_SKELETON_RIGID:{
        if (_IsBoxVisible(m_pVisual,mTransform)){
            CKinematics* pV		= smart_cast<CKinematics*>(m_pVisual); VERIFY(pV);
            if (fis_zero(m_fLOD,EPS)&&pV->m_lod){
		        if (_IsValidShader(pV->m_lod,priority,strictB2F)){
	                RCache.set_Shader		(pV->m_lod->shader?pV->m_lod->shader: EDevice->m_WireShader);
    	            RCache.set_xform_world	(mTransform);
        	        pV->m_lod->Render		(1.f);
                }
            }else{
                I = pV->children.begin		();
                E = pV->children.end		();
                for (; I!=E; I++){
                    if (_IsValidShader(*I,priority,strictB2F)){
                        RCache.set_Shader		((*I)->shader?(*I)->shader: EDevice->m_WireShader);
                        RCache.set_xform_world	(mTransform);
                        (*I)->Render		 	(m_fLOD);
                    }
                }
            }
        }
    }break;
    case MT_HIERRARHY:{
        if (_IsBoxVisible(m_pVisual,mTransform)){
            FHierrarhyVisual* pV		= smart_cast<FHierrarhyVisual*>(m_pVisual); VERIFY(pV);
            I = pV->children.begin		();
            E = pV->children.end		();
            for (; I!=E; I++){
		        if (_IsValidShader(*I,priority,strictB2F)){
	                RCache.set_Shader		((*I)->shader?(*I)->shader: EDevice->m_WireShader);
    	            RCache.set_xform_world	(mTransform);
        	        (*I)->Render		 	(m_fLOD);
                }
            }
        }
    }break;
    case MT_PARTICLE_GROUP:{
		PS::CParticleGroup* pG = smart_cast<PS::CParticleGroup*>(m_pVisual); VERIFY(pG);
		xrCriticalSectionGuard guard(pG->onframe_lock);
		for (PS::CParticleGroup::SItem& item : pG->items)
		{
			if (item.root_effect)
				Render(item.root_effect, Fidentity, priority, strictB2F, m_fLOD);

			xrCriticalSectionGuard guard(item.childs_cs);
			for (dxRender_Visual* pEffect : item.children_related)
				Render(pEffect, Fidentity, priority, strictB2F, m_fLOD);

			for (dxRender_Visual* pEffect : item.children_free)
				Render(pEffect, Fidentity, priority, strictB2F, m_fLOD);
		}
    }break;
    case MT_PARTICLE_EFFECT:{
//		if (_IsBoxVisible(m_pVisual,mTransform))
        {
            if (_IsValidShader(m_pVisual,priority,strictB2F)){
                RCache.set_Shader			(m_pVisual->shader?m_pVisual->shader: EDevice->m_WireShader);
                RCache.set_xform_world		(mTransform);
                m_pVisual->Render		 	(m_fLOD);
            }
        }
    }break;
    default:
        if (_IsBoxVisible(m_pVisual,mTransform)){
            if (_IsValidShader(m_pVisual,priority,strictB2F)){
                RCache.set_Shader			(m_pVisual->shader?m_pVisual->shader: EDevice->m_WireShader);
                RCache.set_xform_world		(mTransform);
                m_pVisual->Render		 	(m_fLOD);
            }
        }
        break;
    }
}

void 	CModelPool::RenderSingle(dxRender_Visual* m_pVisual, const Fmatrix& mTransform, float m_fLOD)
{
	for (int p=0; p<4; p++){
    	Render(m_pVisual,mTransform,p,false,m_fLOD);
    	Render(m_pVisual,mTransform,p,true,m_fLOD);
	}
}
void CModelPool::OnDeviceDestroy()
{
	Destroy();
}
#endif