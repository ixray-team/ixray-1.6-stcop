#include "stdafx.h"
#include "XRayRenderVisual.h"

CDS0_RenderVisual::~CDS0_RenderVisual()
{
}
void CDS0_RenderVisual::Load(const char* N, IReader* data, u32 dwFlags)
{
#ifdef DEBUG
	DebugName = N;
#endif

	// header
	VERIFY(data);
	ogf_header	hdr;
	if (data->r_chunk_safe(OGF_HEADER, &hdr, sizeof(hdr)))
	{
		R_ASSERT2(hdr.format_version == xrOGF_FormatVersion, "Invalid visual version");
		
		Type = hdr.type;
	//	if (hdr.shader_id)	Shader = GRenderInterface.GetShader(hdr.shader_id);
		Vis.box.set(hdr.bb.min, hdr.bb.max);
		Vis.sphere.set(hdr.bs.c, hdr.bs.r);
	}
	else {
		FATAL("Invalid visual");
	}

	// Shader
	if (data->find_chunk(OGF_TEXTURE)) {
		string256		fnT, fnS;
		data->r_stringZ(fnT, sizeof(fnT));
		data->r_stringZ(fnS, sizeof(fnS));
		//GResourcesManager->CompileBlender(Shader, fnS, fnT);
	}
}
#define PCOPY(a)	a = pFrom->a
void CDS0_RenderVisual::Copy(CDS0_RenderVisual* pFrom)
{
	PCOPY(Type);
	PCOPY(Vis);
#ifdef DEBUG
	PCOPY(DebugName);
#endif
}
u32 CDS0_RenderVisual::getType()
{
	return Type;
}

vis_data&  CDS0_RenderVisual::getVisData()
{
	return Vis;
}

shared_str CDS0_RenderVisual::getDebugName()
{
	return DebugName;
}
