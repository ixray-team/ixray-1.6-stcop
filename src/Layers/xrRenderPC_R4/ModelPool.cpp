#include "stdafx.h"


#include "ModelPool.h"
#include "../../xrCore/BackgroundLoad.h"

#include "../../xrEngine/Fmesh.h"
#include "../../xrEngine/IGame_Persistent.h"
#	include "FLOD.h"
#	include "FTreeVisual.h"

#include "FVisual.h"
#include "FProgressive.h"
#include "ParticleEffect.h"
#include "ParticleGroup.h"
#include "SkeletonX.h"
#include "FHierrarhyVisual.h"
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
	case MT_LOD:
	{
		V	= new FLOD					();
		break;
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
	default:
		FATAL	("Unknown visual type");
		break;
	}
	R_ASSERT	(V);
	V->Type		= type;
	return		V;
}

namespace
{
	thread_local bool g_AllowChildrenDuplicate = true;
	thread_local bool g_ForceDiscard = false;

	class FlagScope
	{
	public:
		FlagScope(bool& Flag, bool Value) : Target(Flag), Previous(Flag) { Target = Value; }
		~FlagScope() { Target = Previous; }

	private:
		bool& Target;
		bool Previous;
	};

	thread_local bool g_ModelLoaderProfiled = false;

	void MakeModelKey(string_path& Key, const char* Name)
	{
		xr_strcpy(Key, Name);
		_strlwr(Key);
		if (strext(Key))
			*strext(Key) = 0;
	}
}

CModelPool::LoadContext CModelPool::CaptureLoadContext()
{
	LoadContext Context;
	Context.AllowChildrenDuplicate = g_AllowChildrenDuplicate;
	return Context;
}

CModelPool::LoadContextScope::LoadContextScope(const LoadContext& Context)
	: Previous(CaptureLoadContext())
{
	g_AllowChildrenDuplicate = Context.AllowChildrenDuplicate;
}

CModelPool::LoadContextScope::~LoadContextScope()
{
	g_AllowChildrenDuplicate = Previous.AllowChildrenDuplicate;
}

bool CModelPool::IsAsyncLoadEnabled()
{
	return ps_r__common_flags.test(RFLAG_MT_MODEL_LOAD) && !Device.IsEditorMode();
}

void CModelPool::Request(const char* name, EModelRequestPriority Priority)
{
	if (!IsAsyncLoadEnabled() || name == nullptr || name[0] == 0)
		return;

	string_path Name;
	MakeModelKey(Name, name);
	const shared_str Key = Name;
	{
		xrSRWLockGuard Guard(ModelsLock, true);
		if (Models.find(Key) != Models.end())
			return;
	}
	const u32 MaxLoaders = BackgroundLoad::WorkerCount();
	{
		xrCriticalSectionGuard Guard(RequestedLock);
		auto QueuedIt = Queued.find(Key);
		if (QueuedIt != Queued.end())
		{
			if (u8(Priority) < u8(QueuedIt->second))
			{
				xr_deque<shared_str>& From = RequestQueues[u8(QueuedIt->second)];
				From.erase(std::find(From.begin(), From.end(), Key));
				RequestQueues[u8(Priority)].push_back(Key);
				QueuedIt->second = Priority;
			}
			return;
		}

		if (!Requested.insert(Key).second)
			return;

		Queued.emplace(Key, Priority);
		RequestQueues[u8(Priority)].push_back(Key);
		if (ActiveLoaders >= MaxLoaders)
			return;
		ActiveLoaders++;
	}

	RequestTasks.run([this]() { RunLoader(); });
}

void CModelPool::RunLoader()
{
	if (!g_ModelLoaderProfiled && !g_AppInfo.IsPrimaryThread())
	{
		g_ModelLoaderProfiled = true;
		PROF_REGISTER_THREAD("Model loader");
	}

	xrBackgroundWorkScope Background;
	xrTaskBlockingScope Blocking;
	while (true)
	{
		shared_str Key;
		bool IsMoreLoaders = false;
		{
			xrCriticalSectionGuard Guard(RequestedLock);
			xr_deque<shared_str>* Queue = nullptr;
			for (xr_deque<shared_str>& Candidate : RequestQueues)
			{
				if (!Candidate.empty())
				{
					Queue = &Candidate;
					break;
				}
			}

			if (Queue == nullptr)
			{
				ActiveLoaders--;
				return;
			}
			Key = Queue->front();
			Queue->pop_front();
			Queued.erase(Key);

			const size_t Pending = RequestQueues[0].size() + RequestQueues[1].size() + RequestQueues[2].size();
			if (Pending > 0 && ActiveLoaders < BackgroundLoad::WorkerCount())
			{
				ActiveLoaders++;
				IsMoreLoaders = true;
			}
		}

		if (IsMoreLoaders)
		{
			RequestTasks.run([this]() { RunLoader(); });
		}

		{
			PROF_EVENT("CModelPool::Request");
			ModelEntry* Entry = AcquireBase(*Key, nullptr);
			Entry->Refs--;
		}

		xrCriticalSectionGuard Guard(RequestedLock);
		Requested.erase(Key);
	}
}

EModelLoadState CModelPool::GetState(const char* name)
{
	string_path Name;
	MakeModelKey(Name, name);
	const shared_str Key = Name;
	{
		xrSRWLockGuard Guard(ModelsLock, true);
		auto It = Models.find(Key);
		if (It != Models.end())
		{
			const ModelEntry* Entry = It->second;
			if (!Entry->IsReady.load(std::memory_order_acquire))
				return EModelLoadState::Loading;
			return Entry->Model ? EModelLoadState::Ready : EModelLoadState::Failed;
		}
	}

	xrCriticalSectionGuard Guard(RequestedLock);
	return Requested.contains(Key) ? EModelLoadState::Loading : EModelLoadState::NotRequested;
}

void CModelPool::WaitRequests()
{
	PROF_EVENT("CModelPool::WaitRequests");
	RequestTasks.wait();
	WaitAll();
}

dxRender_Visual* CModelPool::DuplicateInstance(dxRender_Visual* V)
{
	PROF_EVENT("CModelPool::Instance_Duplicate");
	R_ASSERT(V);
	dxRender_Visual* N		= Instance_Create(V->Type);
	N->Copy			(V);
	N->Spawn		();
	return N;
}

dxRender_Visual*	CModelPool::Instance_Duplicate	(dxRender_Visual* V)
{
	R_ASSERT(V);
	{
		// inc ref counter
		xrSRWLockGuard Guard(ModelsLock, true);
		auto It = ModelsByVisual.find(V);
		if (It != ModelsByVisual.end())
			It->second->Refs++;
	}
	return DuplicateInstance(V);
}

CModelPool::ModelEntry* CModelPool::AcquireBase(const char* Name, IReader* Data)
{
	const shared_str Key = Name;
	ModelEntry* Entry = nullptr;
	{
		xrSRWLockGuard Guard(ModelsLock, true);
		auto It = Models.find(Key);
		if (It != Models.end())
		{
			Entry = It->second;
			Entry->Refs++;
		}
	}

	bool IsOwner = false;
	if (Entry == nullptr)
	{
		xrSRWLockGuard Guard(ModelsLock);
		ModelEntry*& Slot = Models[Key];
		if (Slot == nullptr)
		{
			Slot = new ModelEntry();
			Slot->Name = Key;
			Slot->LoadGuard.Enter();
			IsOwner = true;
		}
		Entry = Slot;
		Entry->Refs++;
	}

	if (!IsOwner)
	{
		if (!Entry->IsReady.load(std::memory_order_acquire))
		{
			PROF_EVENT("CModelPool: wait for a pending load");
			xrCriticalSectionGuard Wait(Entry->LoadGuard);
		}
		return Entry;
	}

	dxRender_Visual* V = nullptr;
	{
		FlagScope Scope(g_AllowChildrenDuplicate, false);
		V = Data ? Instance_Load(Name, Data) : Instance_Load(Name);
	}

	{
		xrSRWLockGuard Guard(ModelsLock);
		Entry->Model = V;
		if (V != nullptr)
			ModelsByVisual[V] = Entry;
	}

	Entry->IsReady.store(true, std::memory_order_release);
	Entry->LoadGuard.Leave();
	return Entry;
}

void CModelPool::DestroyBase(ModelEntry* Entry)
{
	if (Entry->Model != nullptr)
	{
		FlagScope Scope(g_ForceDiscard, true);
		Entry->Model->Release();
		xr_delete(Entry->Model);
	}
	xr_delete(Entry);
}

void CModelPool::WaitAll()
{
	xr_vector<ModelEntry*> Pending;
	{
		xrSRWLockGuard Guard(ModelsLock, true);
		for (const auto& [Name, Entry] : Models)
		{
			if (!Entry->IsReady.load(std::memory_order_acquire))
				Pending.push_back(Entry);
		}
	}

	for (ModelEntry* Entry : Pending)
	{
		xrCriticalSectionGuard Wait(Entry->LoadGuard);
	}
}

dxRender_Visual*	CModelPool::Instance_Load		(const char* InputName)
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
				if (Device.IsEditorMode())
				{
					Msg("! Can't find model file '%s'.",name);
				}
				else
				{
					Debug.fatal(DEBUG_INFO,"Can't find model file '%s'.",name);
				}
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
	return V;
}

dxRender_Visual* CModelPool::Instance_Load(const char* name, IReader* data)
{
	PROF_EVENT(__FUNCTION__);
	dxRender_Visual	*V;

	ogf_header			H;
	data->r_chunk_safe	(OGF_HEADER,&H,sizeof(H));
	V = Instance_Create (H.type);
	V->Load				(name,data,0);
	return V;
}

void CModelPool::Destroy()
{
	WaitRequests();

	// Pool
	{
		xrCriticalSectionGuard Guard(PoolLock);
		Pool.clear();
	}

	// Registry
	while (true)
	{
		dxRender_Visual* V = nullptr;
		{
			xrCriticalSectionGuard Guard(PoolLock);
			if (Registry.empty())
				break;
			V = Registry.begin()->first;
		}
		DeleteInternal	(V,true);
	}

	// Base/Reference
	xr_vector<ModelEntry*> Entries;
	{
		xrSRWLockGuard Guard(ModelsLock);
		Entries.reserve(Models.size());
		for (const auto& [Name, Entry] : Models)
			Entries.push_back(Entry);
		Models.clear();
		ModelsByVisual.clear();
	}

	for (ModelEntry* Entry : Entries)
	{
		if (Entry->Model != nullptr)
		{
			Entry->Model->Release();
			xr_delete(Entry->Model);
		}
		xr_delete(Entry);
	}

	// cleanup motions container
	g_pMotionsContainer->clean(false);
}

CModelPool::CModelPool()
{
	g_pMotionsContainer		= new motions_container();
}

CModelPool::~CModelPool()
{
	Destroy					();
	xr_delete				(g_pMotionsContainer);
}

dxRender_Visual* CModelPool::Instance_Find(const char* N)
{
	PROF_EVENT(__FUNCTION__);
	xrSRWLockGuard Guard(ModelsLock, true);
	auto It = Models.find(shared_str(N));
	if (It == Models.end() || !It->second->IsReady.load(std::memory_order_acquire))
		return nullptr;
	return It->second->Model;
}

dxRender_Visual* CModelPool::Create(const char* name, IReader* data)
{
	if (Device.IsEditorMode() && (!name||!name[0]))	return nullptr;
	string_path low_name;	VERIFY	(xr_strlen(name)<sizeof(low_name));
	xr_strcpy(low_name,name);	_strlwr	(low_name);
	if (strext(low_name))	*strext	(low_name)=0;
//	Msg						("-CREATE %s",low_name);

	// 0. Search POOL
	dxRender_Visual* Model = nullptr;
	{
		xrCriticalSectionGuard Guard(PoolLock);
		POOL_IT	it			=	Pool.find	(low_name);
		if (it!=Pool.end())
		{
			Model			= it->second;
			Pool.erase		(it);
		}
	}

	if (Model != nullptr)
	{
		// 1. Instance found
		Model->Spawn		();
		return Model;
	}

	ModelEntry* Entry = AcquireBase(low_name, data);
	if (Entry->Model == nullptr)
	{
		Entry->Refs--;
		return nullptr;
	}

	// 3. If found - return (cloned) reference
	Model = DuplicateInstance(Entry->Model);
	{
		xrCriticalSectionGuard Guard(PoolLock);
		Registry.insert(std::make_pair(Model, shared_str(low_name)));
	}
	return Model;
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
		if (data)		Base = Instance_Load	(low_name,data);
		else			Base = Instance_Load	(low_name);
	}

    dxRender_Visual* Model	= g_AllowChildrenDuplicate?Instance_Duplicate(Base):Base;
    return					Model;
}

extern  xr_atomic_bool ENGINE_API g_bRendering;
void	CModelPool::DeleteInternal	(dxRender_Visual* &V, bool bDiscard)
{
	VERIFY					(!g_bRendering);
    if (!V)					return;
	V->Depart				();
	if (bDiscard||g_ForceDiscard){
		Discard	(V, true);
	}else{
		bool IsPooled = false;
		{
			xrCriticalSectionGuard Guard(PoolLock);
			REGISTRY_IT	it		= Registry.find	(V);
			if (it!=Registry.end())
			{
				// Registry entry found - move it to pool
				Pool.insert			(std::make_pair(it->second,V));
				IsPooled			= true;
			}
		}

		if (!IsPooled)
		{
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
		xrCriticalSectionGuard Guard(PoolLock);
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
	xr_vector<dxRender_Visual*> Queue;
	{
		xrCriticalSectionGuard Guard(PoolLock);
		Queue.swap(ModelsToDelete);
	}

	for (dxRender_Visual*& V : Queue)
		DeleteInternal(V);
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
	shared_str name;
	{
		xrCriticalSectionGuard Guard(PoolLock);
		REGISTRY_IT	it = Registry.find(V);
		if (it != Registry.end())
		{
			name = it->second;
			Registry.erase(it);
		}
	}

	if (name.size())
	{
		// Base
		ModelEntry* Removed = nullptr;
		{
			xrSRWLockGuard Guard(ModelsLock);
			auto I = Models.find(name);
			if (I != Models.end())
			{
				ModelEntry* Entry = I->second;
				if (b_complete || strchr(*name, '#'))
				{
					VERIFY(Entry->Refs > 0);
					if (0 == --Entry->Refs)
					{
						Removed = Entry;
						ModelsByVisual.erase(Entry->Model);
						Models.erase(I);
					}
				}
				else if (Entry->Refs > 0)
				{
					Entry->Refs--;
				}
			}
		}

		if (Removed != nullptr)
			DestroyBase(Removed);
	}

	// Registry entry not-found - just special type of visual / particles / etc.
	xr_delete(V);
	V = nullptr;
}

void CModelPool::Prefetch()
{
	if (IsAsyncLoadEnabled())
	{
		xrSRWLockGuard Guard(ModelsLock, true);
		Msg("* Model pool: %u base models, prefetch visuals load in the background", u32(Models.size()));
		return;
	}

	const xr_string section = g_pGamePersistent->GetPrefetchSection("prefetch_visuals_");
	CInifile::Sect& sect = pSettings->r_section(section.c_str());
	for (CInifile::SectCIt I = sect.Data.begin(); I != sect.Data.end(); I++)
	{
		const CInifile::Item& item = *I;
		dxRender_Visual* V = Create(item.first.c_str());
		Delete(V, false);
	}

	xrSRWLockGuard Guard(ModelsLock, true);
	Msg("* Model pool: %u base models", u32(Models.size()));
}

void CModelPool::ClearPool( bool b_complete)
{
	WaitRequests();

	POOL Taken;
	{
		xrCriticalSectionGuard Guard(PoolLock);
		Taken.swap(Pool);
	}

	for (auto& [Name, V] : Taken)
		Discard(V, b_complete);
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

// Editor draw path (GEditorDraw) exists only in D3D11 builds.
#include "EditorDrawContext.h"

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
	                RCache.set_Shader		(pV->m_lod->shader?pV->m_lod->shader: GEditorDraw.WireShader);
    	            RCache.set_xform_world	(mTransform);
        	        pV->m_lod->Render		(1.f);
                }
            }else{
                I = pV->children.begin		();
                E = pV->children.end		();
                for (; I!=E; I++){
                    if (_IsValidShader(*I,priority,strictB2F)){
                        RCache.set_Shader		((*I)->shader?(*I)->shader: GEditorDraw.WireShader);
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
	                RCache.set_Shader		((*I)->shader?(*I)->shader: GEditorDraw.WireShader);
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
                RCache.set_Shader			(m_pVisual->shader?m_pVisual->shader: GEditorDraw.WireShader);
                RCache.set_xform_world		(mTransform);
                m_pVisual->Render		 	(m_fLOD);
            }
        }
    }break;
    default:
        if (_IsBoxVisible(m_pVisual,mTransform)){
            if (_IsValidShader(m_pVisual,priority,strictB2F)){
                RCache.set_Shader			(m_pVisual->shader?m_pVisual->shader: GEditorDraw.WireShader);
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
