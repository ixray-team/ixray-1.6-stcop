// dxRender_Visual.cpp: implementation of the dxRender_Visual class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"


#ifndef _EDITOR
#	include "../../xrEngine/Render.h"
#endif // #ifndef _EDITOR

#include "FBasicVisual.h"
#include "../../xrEngine/Fmesh.h"
#include "src/xrCore/SharedMaterialLibrary.h"

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
}

void dxRender_Visual::Release		()
{
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
		if (hdr.shader_id)
		{
			shader	= ::RImplementation.getShader	(hdr.shader_id);
		}
		vis.box.set			(hdr.bb.min,hdr.bb.max	);
		vis.sphere.set		(hdr.bs.c,	hdr.bs.r	);
	} else {
		FATAL				("Invalid visual");
	}

	// Shader
	bool SharedMat = false;
	if (data->find_chunk(OGF_SHARED_MATERIAL_SETTINGS))
	{
		SharedMat = data->r_u8();
	}
	if (SharedMat)
	{
		string128 Name;
		data->r_stringZ(Name, sizeof(Name));
		auto MatData = CSharedMaterialLibrary::Instance().GetData(Name);
		xr_string ShaderDescr = MatData->m_ShaderName.c_str();
		auto ShaderEndPos = ShaderDescr.size();
		ShaderDescr.append("/");
		ShaderDescr.append(MatData->m_Texture.c_str());
		xr_stack_string512 lmap_tex;
		data->r_stringZ(lmap_tex.data(), lmap_tex.Length);
		ShaderDescr.append(lmap_tex.c_str());
		shader = ::RImplementation.getShaderShared(ShaderDescr.c_str());
		ShaderDescr[ShaderEndPos] = '\0';
		
		shader.create(ShaderDescr.c_str(),ShaderDescr.c_str()+ShaderEndPos+1);
	}
	else if (data->find_chunk(OGF_TEXTURE)) {
		string256		fnT,fnS;
		data->r_stringZ	(fnT,sizeof(fnT));
		data->r_stringZ	(fnS,sizeof(fnS));
		shader.create	(fnS,fnT);
	}

    // desc
#ifdef _EDITOR
    if (data->find_chunk(OGF_S_DESC)) 
	    desc.Load		(*data);
#endif
}

#define PCOPY(a)	a = pFrom->a
void	dxRender_Visual::Copy(dxRender_Visual *pFrom)
{
	PCOPY(Type);
	PCOPY(shader);
	PCOPY(vis);
#ifdef _EDITOR
	PCOPY(desc);
#endif
	PCOPY(dbg_name);
}
