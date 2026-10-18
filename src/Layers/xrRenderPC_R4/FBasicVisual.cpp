// dxRender_Visual.cpp: implementation of the dxRender_Visual class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#include "../../xrEngine/Render.h"

#include "FBasicVisual.h"
#include "../../xrEngine/Fmesh.h"
#include "SkeletonX.h"
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

IRender_Mesh::~IRender_Mesh()		
{ 
	_RELEASE(p_rm_Vertices); 
	_RELEASE(p_rm_Indices);		
}

dxRender_Visual::dxRender_Visual		()
{
	Type				= 0;
	shader				= nullptr;
	vis.clear			();
}

dxRender_Visual::~dxRender_Visual		()
{
	if (IsSpotShadowCacheReferenced.load(std::memory_order_relaxed) && RImplementation.Target)
	{
		// ModelPool defers destruction until rendering ends. Clear snapshots at the next light boundary.
		RImplementation.Target->InvalidateSpotShadowCache();
	}
}

void dxRender_Visual::Release		()
{
	if (IsSpotShadowCacheReferenced.load(std::memory_order_relaxed) && RImplementation.Target)
	{
		RImplementation.Target->InvalidateSpotShadowCache();
	}
}

//CStatTimer						tscreate;

void dxRender_Visual::Load		(const char* N, IReader *data, u32 )
{
	dbg_name = N;

	// header
	VERIFY		(data);
	ogf_header	hdr;
	if (data->r_chunk_safe(OGF_HEADER,&hdr,sizeof(hdr)))
	{
		R_ASSERT2			(hdr.format_version==xrOGF_FormatVersion, "Invalid visual version");
		Type				= hdr.type;
		//if (hdr.shader_id)	shader	= ::Render->getShader	(hdr.shader_id);
		if (hdr.shader_id)	shader	= ::RImplementation.getShader	(hdr.shader_id);
		vis.box.set			(hdr.bb.min,hdr.bb.max	);
		vis.sphere.set		(hdr.bs.c,	hdr.bs.r	);
	} else {
		FATAL				("Invalid visual");
	}

	// Shader
	if (data->find_chunk(OGF_TEXTURE)) {
		string256		fnT,fnS;
		data->r_stringZ	(fnT,sizeof(fnT));
		data->r_stringZ	(fnS,sizeof(fnS));

		texture_name = fnT;
		orig_texture_name = fnT;

		shader_name = fnS;
		orig_shader_name = fnS;

		shader.create(fnS, fnT);
	}

    // desc
    if (Device.IsEditorMode() && data->find_chunk(OGF_S_DESC))
	    desc.Load		(*data);
}

ICF int RenderMode_to_SkinningMode(u32 rm)
{
	return (rm == CSkeletonX::RM_SKINNING_SOFT) ? -1 : int(rm) - 1;
}
void dxRender_Visual::reload_shader()
{
	int old_mode = Engine.External.GetSkinningMode();
	if (CSkeletonX* S = smart_cast<CSkeletonX*>(this))
	{
		Engine.External.SetSkinningMode(RenderMode_to_SkinningMode(S->RenderMode));
	}
	shader.create(*shader_name, *texture_name);
	Engine.External.SetSkinningMode(old_mode);
	if (IsSpotShadowCacheReferenced.load(std::memory_order_relaxed) && RImplementation.Target)
	{
		RImplementation.Target->InvalidateSpotShadowCache();
	}
}
void dxRender_Visual::restore_shader()
{
	int old_mode = Engine.External.GetSkinningMode();
	if (CSkeletonX* S = smart_cast<CSkeletonX*>(this))
	{
		Engine.External.SetSkinningMode(RenderMode_to_SkinningMode(S->RenderMode));
	}
	shader.create(*orig_shader_name, *texture_name);
	Engine.External.SetSkinningMode(old_mode);
	if (IsSpotShadowCacheReferenced.load(std::memory_order_relaxed) && RImplementation.Target)
	{
		RImplementation.Target->InvalidateSpotShadowCache();
	}
}
void dxRender_Visual::restore_texture()
{
	int old_mode = Engine.External.GetSkinningMode();
	if (CSkeletonX* S = smart_cast<CSkeletonX*>(this))
	{
		Engine.External.SetSkinningMode(RenderMode_to_SkinningMode(S->RenderMode));
	}
	shader.create(*shader_name, *orig_texture_name);
	Engine.External.SetSkinningMode(old_mode);
	if (IsSpotShadowCacheReferenced.load(std::memory_order_relaxed) && RImplementation.Target)
	{
		RImplementation.Target->InvalidateSpotShadowCache();
	}
}

#define PCOPY(a)	a = pFrom->a
void	dxRender_Visual::Copy(dxRender_Visual *pFrom)
{
	PCOPY(Type);
	PCOPY(orig_texture_name);
	PCOPY(texture_name);
	PCOPY(orig_shader_name);
	PCOPY(shader_name);
	PCOPY(shader);
	PCOPY(vis);
	PCOPY(desc);
	PCOPY(dbg_name);
}
