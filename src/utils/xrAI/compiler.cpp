#include "StdAfx.h"
#include "compiler.h"

#include "xrCore/SharedMaterialLibrary.h"

IComputeData comp_data;
Nodes g_nodes;
SAIParams g_params;

b_texture& IComputeData::GetTexture(u32 ID, bool Shared)
{
	if (Shared)
	{
		return comp_data.g_textures_shared[&comp_data.g_materials_shared[ID]];
	}
	return comp_data.g_textures[comp_data.g_materials[ID].surfidx];
}

Shader_xrLC& IComputeData::GetShaderXRLC(u32 ID, bool Shared)
{
	if (Shared)
	{
		return *comp_data.g_shaders_xrlc->Get(
			CSharedMaterialLibrary::Instance().GetData(
				comp_data.g_materials_shared[ID].Name)->m_ShaderXRLCName.c_str()
				);
	}
	return *comp_data.g_shaders_xrlc->Get(comp_data.g_shader_compile[comp_data.g_materials[ID].surfidx].name);
}

void vertex::PointLF(Fvector& D)
{
	Fvector	d;	d.set(0,-1,0);
	Fvector	v	= Pos;	
	float	s	= g_params.fPatchSize/2;
	v.x			-= s;
	v.z			+= s;
	Plane.intersectRayPoint(v,d,D);
}

void vertex::PointFR(Fvector& D)
{
	Fvector	d;	d.set(0,-1,0);
	Fvector	v	= Pos;	
	float	s	= g_params.fPatchSize/2;
	v.x			+= s;
	v.z			+= s;
	Plane.intersectRayPoint(v,d,D);
}

void vertex::PointRB(Fvector& D)
{
	Fvector	d;	d.set(0,-1,0);
	Fvector	v	= Pos;	
	float	s	= g_params.fPatchSize/2;
	v.x			+= s;
	v.z			-= s;
	Plane.intersectRayPoint(v,d,D);
}

void vertex::PointBL(Fvector& D)
{
	Fvector	d;	d.set(0,-1,0);
	Fvector	v	= Pos;	
	float	s	= g_params.fPatchSize/2;
	v.x			-= s;
	v.z			-= s;
	Plane.intersectRayPoint(v,d,D);
}

void mem_Optimize()
{
	Memory.mem_compact();
	Msg("* Memory usage: %d M", Memory.mem_usage() / (1024 * 1024));
}

void xrCompiler(const char* name, bool draft_mode, bool pure_covers, bool skipThm, const char* out_name)
{
	Phase("Loading level...");
	xrLoad(name, draft_mode, skipThm);
	mem_Optimize();

	if (!draft_mode) 
	{
		Phase("Calculating coverage...");
		xrCover(pure_covers);
		mem_Optimize();
	}

	Phase("Saving nodes...");
	xrSaveNodes(name, out_name);
	mem_Optimize();

	g_nodes.clear();
	g_nodes.shrink_to_fit();

	comp_data.xrUnload();
 }
