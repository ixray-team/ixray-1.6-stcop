// FHierrarhyVisual.cpp: implementation of the FHierrarhyVisual class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "FHierrarhyVisual.h"
#include "ModelPool.h"
#include "../../xrEngine/Fmesh.h"

#include "../../xrEngine/Render.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

FHierrarhyVisual::FHierrarhyVisual()  : dxRender_Visual()
{
	bDontDelete	= false;
}

FHierrarhyVisual::~FHierrarhyVisual()
{
	if (!bDontDelete) {
		for (u32 i=0; i<children.size(); i++)	
			::Render->model_Delete((IRenderVisual *&)children[i]);
	}
	children.clear();
}

void FHierrarhyVisual::Release()
{
	dxRender_Visual::Release();
	if (!bDontDelete) {
		for (u32 i=0; i<children.size(); i++)
			children[i]->Release();
	}
}

void FHierrarhyVisual::Load(const char* N, IReader *data, u32 dwFlags)
{
	PROF_EVENT(__FUNCTION__);
	dxRender_Visual::Load(N,data,dwFlags);
	if (data->find_chunk(OGF_CHILDREN_L)) 
	{
		// From Link
		u32 cnt = data->r_u32		();
		children.resize				(cnt);
		for (u32 i=0; i<cnt; i++)	{
			u32 ID	= data->r_u32();
			children[i]	= (dxRender_Visual*)::Render->getVisual(ID);
		}
		bDontDelete = true;
	}
	else
	{	
    	if (data->find_chunk(OGF_CHILDREN))
		{
			// From stream
            IReader* OBJ = data->open_chunk(OGF_CHILDREN);
            if (OBJ){
				xr_vector<IReader*> Chunks;
				for (IReader* O = OBJ->open_chunk(0); O; O = OBJ->open_chunk(u32(Chunks.size())))
					Chunks.push_back(O);

				string_path short_name;
				xr_strcpy(short_name, N);
				if (strext(short_name)) *strext(short_name) = 0;

				children.resize(children.size() + Chunks.size());
				const size_t First = children.size() - Chunks.size();
				const auto LoadChild = [&](size_t Index)
				{
					string_path name_load, num;
					xr_strconcat(name_load, short_name, ":", _itoa(int(Index + 1), num, 10));
					children[First + Index] = (dxRender_Visual*)::Render->model_CreateChild(name_load, Chunks[Index]);
					Chunks[Index]->close();
				};

				if (Chunks.size() > 1 && CModelPool::IsAsyncLoadEnabled())
				{
					const CModelPool::LoadContext Context = CModelPool::CaptureLoadContext();
					xr_parallel_for(size_t(0), Chunks.size(), [&](size_t Index)
					{
						xrTaskBlockingScope Blocking;
						CModelPool::LoadContextScope Scope(Context);
						LoadChild(Index);
					});
				}
				else
				{
					for (size_t Index = 0; Index < Chunks.size(); ++Index)
						LoadChild(Index);
				}
                OBJ->close();
            }
			bDontDelete = false;
        }
		else
		{
			FATAL		("Invalid visual");
    	}
	}
}

void	FHierrarhyVisual::Copy(dxRender_Visual *pSrc)
{
	dxRender_Visual::Copy	(pSrc);

	FHierrarhyVisual	*pFrom = (FHierrarhyVisual *)pSrc;

	children.clear	();
	children.reserve(pFrom->children.size());
	for (u32 i=0; i<pFrom->children.size(); i++) {
		dxRender_Visual *p = (dxRender_Visual*) ::Render->model_Duplicate	(pFrom->children[i]);
		children.push_back(p);
	}
	bDontDelete = false;
}
