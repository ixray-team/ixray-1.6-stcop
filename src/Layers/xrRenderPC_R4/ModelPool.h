// ModelPool.h: interface for the CModelPool class.
//////////////////////////////////////////////////////////////////////
#ifndef ModelPoolH
#define ModelPoolH
#pragma once
#include "RenderAPI.h"

// refs
class  dxRender_Visual;
namespace PS	{ 
	struct  SEmitter; 
};

// defs
class RENDER_API CModelPool
{
private:
	friend class CRender;

	struct str_pred {
		IC bool operator()(const shared_str& x, const shared_str& y) const
		{	return xr_strcmp(x,y)<0;}
	};

	struct ModelEntry
	{
		shared_str			Name;
		dxRender_Visual*	Model = nullptr;
		xr_atomic_u32		Refs = 0;
		xr_atomic_bool		IsReady = false;
		xrCriticalSection	LoadGuard;
	};

	typedef xr_multimap<shared_str,dxRender_Visual*,str_pred>	POOL;
	typedef POOL::iterator										POOL_IT;
	typedef xr_map<dxRender_Visual*,shared_str>					REGISTRY;
	typedef REGISTRY::iterator									REGISTRY_IT;
private:
	xr_hash_map<shared_str, ModelEntry*>		Models;
	xr_hash_map<dxRender_Visual*, ModelEntry*>	ModelsByVisual;
	xrSRWLock									ModelsLock;

	xrCriticalSection			PoolLock;
	xr_vector<dxRender_Visual*>	ModelsToDelete;		//
	REGISTRY					Registry;			// Just pairing of pointer / Name
	POOL						Pool;				// Unused / Inactive

	xr_set<dxRender_Visual*>	ModelsToDeleteDeffer;		//
	xrCriticalSection			deffered_del_lock;

	xr_task_group				RequestTasks{ ETaskPriority::Background };
	xr_hash_set<shared_str>		Requested;
	xr_deque<shared_str>		RequestQueues[3];
	xr_hash_map<shared_str, EModelRequestPriority>	Queued;
	u32							ActiveLoaders = 0;
	xrCriticalSection			RequestedLock;

	void						RunLoader			();

	void						Destroy	();
	ModelEntry*					AcquireBase			(const char* Name, IReader* Data);
	dxRender_Visual*			DuplicateInstance	(dxRender_Visual* V);
	void						DestroyBase			(ModelEntry* Entry);
	void						WaitAll				();
public:
                            CModelPool			();
	virtual 				~CModelPool			();
	dxRender_Visual*		Instance_Create		(u32 Type);
	dxRender_Visual*		Instance_Duplicate	(dxRender_Visual* V);
	dxRender_Visual*		Instance_Load		(const char* N);
	dxRender_Visual*		Instance_Load		(const char* N, IReader* data);
	dxRender_Visual*		Instance_Find		(const char* N);

	dxRender_Visual*		CreatePE			(PS::CPEDef* source);
	dxRender_Visual*		CreatePG			(PS::CPGDef* source);
	dxRender_Visual*		Create				(const char* name, IReader* data=nullptr);
	dxRender_Visual*		CreateChild			(const char* name, IReader* data);
	void					Delete				(dxRender_Visual* &V, bool bDiscard=false);
	void					DeleteDeffered		(dxRender_Visual* &V);
	void					Discard				(dxRender_Visual* &V, bool b_complete);
	void					DeleteInternal		(dxRender_Visual* &V, bool bDiscard=false);
	void					DeleteQueue			();
	void					DeleteQueuedDeffer	();
	
	static bool				IsAsyncLoadEnabled	();

	struct LoadContext
	{
		bool				AllowChildrenDuplicate = true;
	};
	static LoadContext		CaptureLoadContext	();

	class RENDER_API LoadContextScope
	{
	public:
		explicit LoadContextScope(const LoadContext& Context);
		~LoadContextScope();

	private:
		LoadContext			Previous;
	};

	void					Request				(const char* name, EModelRequestPriority Priority);
	EModelLoadState			GetState			(const char* name);
	void					WaitRequests		();
	void					GetLoaded			(xr_vector<shared_str>& Names);

	void					Prefetch			();
	void					ClearPool			( bool b_complete );

	void					OnDeviceDestroy		();
	void 					Render				(dxRender_Visual* m_pVisual, const Fmatrix& mTransform, int priority, bool strictB2F, float m_fLOD);
	void 					RenderSingle		(dxRender_Visual* m_pVisual, const Fmatrix& mTransform, float m_fLOD);
};
#endif //ModelPoolH
