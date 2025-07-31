#pragma once
#include "XRayRenderVisual.h"

class XRayModelPool
{
	struct str_pred 
	{
		IC bool operator()(const shared_str& x, const shared_str& y) const
		{
			return xr_strcmp(x, y) < 0;
		}
	};
	struct ModelDef
	{
		shared_str			name;
		XRayRenderVisual* model;
		u32					refs;
		ModelDef() { refs = 0; model = 0; }
	};

	typedef xr_multimap<shared_str, XRayRenderVisual*, str_pred>	POOL;
	typedef POOL::iterator										POOL_IT;
	typedef xr_map<XRayRenderVisual*, shared_str>					REGISTRY;
	typedef REGISTRY::iterator									REGISTRY_IT;
private:
	xr_vector<ModelDef>			Models;				// Reference / Base
	xr_vector<XRayRenderVisual*>	ModelsToDelete;		// 
	REGISTRY					Registry;			// Just pairing of pointer / Name
	POOL						Pool;				// Unused / Inactive
	BOOL						bLogging;
	BOOL						bForceDiscard;
	BOOL						bAllowChildrenDuplicate;

	void						Destroy();
public:
	XRayModelPool();
	virtual 				~XRayModelPool();
	XRayRenderVisual*		Instance_Create(u32 Type);
	XRayRenderVisual*		Instance_Duplicate(XRayRenderVisual* V);
	XRayRenderVisual*		Instance_Load(LPCSTR N, BOOL allow_register);
	XRayRenderVisual*		Instance_Load(LPCSTR N, IReader* data, BOOL allow_register);
	void					Instance_Register(LPCSTR N, XRayRenderVisual* V);
	XRayRenderVisual*		Instance_Find(LPCSTR N);

	XRayRenderVisual*		Create(LPCSTR name, IReader* data = 0);
	XRayRenderVisual*		CreateChild(LPCSTR name, IReader* data);
	void					Delete(XRayRenderVisual*& V, BOOL bDiscard = FALSE);
	void					Discard(XRayRenderVisual*& V, BOOL b_complete);
	void					DeleteInternal(XRayRenderVisual*& V, BOOL bDiscard = FALSE);
	void					DeleteQueue();

	void					Logging(BOOL bEnable) { bLogging = bEnable; }

	void					Prefetch();
	void					ClearPool(BOOL b_complete);
	virtual void Render();
};

extern XRayModelPool* GModelPool;