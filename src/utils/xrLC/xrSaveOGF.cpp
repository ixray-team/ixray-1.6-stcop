#include "StdAfx.h"
#include "Build.h"
#include "OGF_Face.h"
#include "../../xrCore/FormatParsers/LevelGeom/GeomIO.h"
#include "../xrForms/CompilersUI.h"

SWIContainer g_SWI,x_SWI;
VBContainer g_VB,x_VB;
IBContainer g_IB,x_IB;

bool CBuild::IsOGFContainersEmpty()
{
	return	g_SWI.is_empty() &&
			x_SWI.is_empty() &&
			g_VB.is_empty()  &&
			x_VB.is_empty()  &&
			g_IB.is_empty()  &&
			x_IB.is_empty() ;
}

static u32 g_batch_count;
static u32 g_batch_verts;
static u32 g_batch_faces;
		   
static u32 g_batch_50;
static u32 g_batch_100;
static u32 g_batch_500;
static u32 g_batch_1000;
static u32 g_batch_5000;

u16 RegisterShader(LPCSTR T)
{
	for (u32 it = 0; it < pBuild->g_Shaders.size(); it++)
	{
		if (0 == _stricmp(T, pBuild->g_Shaders[it]))
			return it;
	}
	
	pBuild->g_Shaders.push_back(xr_strdup(T));
	return (u16)pBuild->g_Shaders.size() - 1;
}

void geom_batch_average(u32 verts, u32 faces)
{
	g_batch_count++;
	g_batch_verts += verts;
	g_batch_faces += faces;

	if (faces <= 50)			g_batch_50++;
	else if (faces <= 100)		g_batch_100++;
	else if (faces <= 500)		g_batch_500++;
	else if (faces <= 1000)		g_batch_1000++;
	else if (faces <= 5000)		g_batch_5000++;
}

static bool	remap_order(u32 id0, u32 id1)
{
	OGF* o0 = (OGF*)g_tree[id0];
	OGF* o1 = (OGF*)g_tree[id1];
	return	xr_strcmp(*o0->textures.front().name, *o1->textures.front().name) < 0;
}

void CBuild::SaveTREE(IWriter& fs)
{
	CMemoryWriter MFS;

	clMsg("Geometry buffers...");
	xr_vector<u32> remap;
	remap.reserve(g_tree.size());

	for (u32 rid = 0; rid < g_tree.size(); rid++)
	{
		if (OGF* o = smart_cast<OGF*>(g_tree[rid]))
		{
			remap.push_back(rid);
		}
	}

	std::stable_sort(remap.begin(), remap.end(), remap_order);
	clMsg("remap-size: %d / %d", remap.size(), g_tree.size());
	for (u32 sid = 0; sid < remap.size(); sid++)
	{
		u32 id = remap[sid];
		g_tree[id]->PreSave(id);
	}

	clMsg("Visuals...");
	fs.open_chunk(fsL_VISUALS);
	for (xr_vector<OGF_Base*>::iterator it = g_tree.begin(); it != g_tree.end(); it++)
	{
		u32 idx = u32(it - g_tree.begin());
		MFS.open_chunk(idx);
		(*it)->Save(MFS);
		MFS.close_chunk();
		Progress(float(idx) / float(g_tree.size()));
	}
	fs.w(MFS.pointer(), MFS.size());
	fs.close_chunk();

	mem_Compact();

	{
		xr_unique_ptr<XRay::Geom::IFormat> FormatPtr = nullptr;
		switch (gCompilerMode.LC_GeomType)
		{
		case GeomVanillaType::Vanilla:
			{
				FormatPtr.reset(new XRay::Geom::CGeomVanillaFormat);
				break;
			}
		case GeomVanillaType::Chunked:
			{
				size_t mem_bytes = g_VB.size() + g_IB.size() + g_SWI.size();
				u32 Number = (mem_bytes/(1024ull*1024ull))/gCompilerMode.LC_GeomChunkSize;
				if (!Number)
				{
					FormatPtr.reset(new XRay::Geom::CGeomVanillaFormat);
				} else
				{
					FormatPtr.reset(new XRay::Geom::CGeomVanillaChunkedFormat(Number+1));
				}
				break;
			}
		default:
			{
				FATAL("Invalid Geom type!");
			}
		}
		IVERIFY(FormatPtr);
		
		g_VB.VerifyForSave();
		FormatPtr->AddVBData(g_VB);
		FormatPtr->AddIBData(g_IB);
		FormatPtr->AddSWIData(g_SWI);
		xr_stack_string_path Path = pBuild->path;
		Path.append("level");
		Write(Path, ".geom", *FormatPtr);
		g_VB.Clear();
		g_IB.Clear();
		g_SWI.Clear();
	}
	{
		xr_unique_ptr<XRay::Geom::IFormat> FormatPtr = nullptr;
		switch (gCompilerMode.LC_GeomType)
		{
		case GeomVanillaType::Vanilla:
			{
				FormatPtr.reset(new XRay::Geom::CGeomVanillaFormat);
				break;
			}
		case GeomVanillaType::Chunked:
			{
				size_t mem_bytes = x_VB.size() + x_IB.size() + x_SWI.size();
				u32 Number = (mem_bytes/(1024ull*1024ull))/gCompilerMode.LC_GeomChunkSize;
				if (!Number)
				{
					FormatPtr.reset(new XRay::Geom::CGeomVanillaFormat);
				} else
				{
					FormatPtr.reset(new XRay::Geom::CGeomVanillaChunkedFormat(Number+1));
				}
				break;
			}
		default:
			{
				FATAL("Invalid Geom type!");
			}
		}
		IVERIFY(FormatPtr);
		
		x_VB.VerifyForSave();
		FormatPtr->AddVBData(x_VB);
		FormatPtr->AddIBData(x_IB);
		FormatPtr->AddSWIData(x_SWI);
		xr_stack_string_path Path = pBuild->path;
		Path.append("level");
		Write(Path, ".geomx", *FormatPtr);
		x_VB.Clear();
		x_IB.Clear();
		x_SWI.Clear();
	}

	clMsg("Shader table...");
	fs.open_chunk(fsL_SHADERS);
	fs.w_u32(g_Shaders.size());
	for (xr_vector<LPCSTR>::iterator T = g_Shaders.begin(); T != g_Shaders.end(); T++)
		fs.w_stringZ(*T);
	fs.close_chunk();

	clMsg("Save OGF ENDED");
}