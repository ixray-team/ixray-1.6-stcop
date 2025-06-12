#pragma once
#include "XRayRenderVisual.h"

class CDS0_ModelPool
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
		CDS0_RenderVisual* model;
		u32					refs;
		ModelDef() { refs = 0; model = 0; }
	};

	typedef xr_multimap<shared_str, CDS0_RenderVisual*, str_pred>	POOL;
	typedef POOL::iterator										POOL_IT;
	typedef xr_map<CDS0_RenderVisual*, shared_str>					REGISTRY;
	typedef REGISTRY::iterator									REGISTRY_IT;

private:
	xr_vector<ModelDef>			Models;				// Reference / Base
	xr_vector<CDS0_RenderVisual*>	ModelsToDelete;		// 
	REGISTRY					Registry;			// Just pairing of pointer / Name
	POOL						Pool;				// Unused / Inactive
	BOOL						bForceDiscard;
	BOOL						bAllowChildrenDuplicate;

	void						Destroy();

public:
	CDS0_ModelPool();
	virtual 				~CDS0_ModelPool();
	CDS0_RenderVisual*		Instance_Create(u32 Type);
	CDS0_RenderVisual*		Instance_Duplicate(CDS0_RenderVisual* V);
	CDS0_RenderVisual*		Instance_Load(LPCSTR N, BOOL allow_register);
	CDS0_RenderVisual*		Instance_Load(LPCSTR N, IReader* data, BOOL allow_register);
	void					Instance_Register(LPCSTR N, CDS0_RenderVisual* V);
	CDS0_RenderVisual*		Instance_Find(LPCSTR N);

	CDS0_RenderVisual*		Create(LPCSTR name, IReader* data = 0);
	CDS0_RenderVisual*		CreateChild(LPCSTR name, IReader* data);
	void					Delete(CDS0_RenderVisual*& V, BOOL bDiscard = FALSE);
	void					Discard(CDS0_RenderVisual*& V, BOOL b_complete);
	void					DeleteInternal(CDS0_RenderVisual*& V, BOOL bDiscard = FALSE);
	void					DeleteQueue();

	void					Prefetch();
	void					ClearPool(BOOL b_complete);
	virtual void Render();
};

extern CDS0_ModelPool* GModelPool;