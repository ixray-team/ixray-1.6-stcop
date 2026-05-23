// Build.cpp: implementation of the CBuild class.
//
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "Build.h"

#include "../xrLC_Light/xrMU_Model.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"
#include "../xrLC_Light/mu_model_light.h"
#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/Lightmap.h"
#include "../xrLC_Light/xrDeflectorDefs.h"

#include "../xrForms/CompilersUI.h"

#ifdef IXR_WINDOWS
#	include <psapi.h>
#endif

void calc_ogf(xrMU_Model& mu_model);
void export_geometry(xrMU_Model& mu_model);
void export_ogf(xrMU_Reference& mu_reference);

extern u16 RegisterShader(const char* T);

// Буферы геометрии
struct OGF_Base;
xr_vector<OGF_Base*> g_tree;
vec2Face			 g_XSplit;

void CBuild::CheckBeforeSave(u32 stage)
{
	bool b_g_tree_empty = g_tree.empty();
	R_ASSERT(b_g_tree_empty);
	bool b_g_XSplit_empty = g_XSplit.empty();
	R_ASSERT(b_g_XSplit_empty);
	bool b_IsOGFContainersEmpty = IsOGFContainersEmpty();
	R_ASSERT(b_IsOGFContainersEmpty);
}

void CBuild::TempSave(u32 stage)
{
	CheckBeforeSave(stage);
}

//////////////////////////////////////////////////////////////////////
#include "../xrLC_Light/embree_raytracing/EmbreeRayTrace.h"
static bool cuda_setuped = false;
static bool embree_setuped = false;

CBuild::CBuild()
{
	lmapNameID = 0;

	if (gCompilerMode.CUDA || gCompilerMode.Embree)
	{
		if (!cuda_setuped || !embree_setuped)
			Phase("[CUDA,EMBREE] Initialize Devices ...");

		// Se7kills Initialize Device Embree
#ifdef LCCUDA_BUILD
		if (gCompilerMode.CUDA && !cuda_setuped)
		{
			cuda_setuped = true;
			GPUTaskinSystem.InitializeGPU();
		}
#endif 
		// На стадии xrMU-Models Нужно !
		if ((gCompilerMode.CUDA || gCompilerMode.Embree) && !embree_setuped)
		{
			embree_setuped = true;
			InitializeEmbreeDevice();
		}
	}
}

#include "OGF_Face.h"
CBuild::~CBuild()
{
	clMsg("[xrLC_Remove] mem usage before: %u mb", GetHeapMemory() / 1024 / 1024);

	destroy_global_data();

	clMsg("[xrLC_Remove] Removing GTree");
	for (auto OGF : g_tree)
	{
		xr_delete(OGF);
	}
	g_tree.clear();
	g_tree.shrink_to_fit();

	clMsg("[xrLC_Remove] Removing g_XSplits !");
	for (auto faces : g_XSplit)
		xr_delete(faces);
	g_XSplit.clear();
	g_XSplit.shrink_to_fit();

	Memory.mem_compact();
	clMsg("[xrLC_Remove] mem usage after: %u  mb", GetHeapMemory() / 1024 / 1024);
}
 
CMemoryWriter&	CBuild::err_invalid()
{
	VERIFY(lc_global_data()); 
	return lc_global_data()->err_invalid(); 
}
CMemoryWriter&	CBuild::err_multiedge()
{
	VERIFY(lc_global_data()); 
	return lc_global_data()->err_multiedge(); 
}
CMemoryWriter	&CBuild::err_tjunction()
{
	VERIFY(lc_global_data()); 
	return lc_global_data()->err_tjunction(); 
}
xr_vector<b_material>&	CBuild::materials()	
{
	VERIFY(lc_global_data()); 
	return lc_global_data()->materials(); 
}
xr_vector<b_BuildTexture>&	CBuild::textures()		
{
	VERIFY(lc_global_data());
	return lc_global_data()->textures(); 
}

base_lighting&	CBuild::L_static()
{
	VERIFY(lc_global_data()); return lc_global_data()->L_static(); 
}

Shader_xrLC_LIB&	CBuild::shaders()		
{
	VERIFY(lc_global_data()); 
	return lc_global_data()->shaders(); 
}

void CBuild::Light_prepare()
{
	for (vecFaceIt I=lc_global_data()->g_faces().begin();	I!=lc_global_data()->g_faces().end(); I++)
		(*I)->CacheOpacity();
	for (u32 m=0; m<mu_models().size(); m++)
		mu_models()[m]->calc_faceopacity();
}

size_t GetHeapMemory()
{
#ifdef IXR_WINDOWS
 	PROCESS_MEMORY_COUNTERS_EX pmc;
	if (GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS*)&pmc, sizeof(pmc)))
	{
 		return pmc.PrivateUsage;
	}
#endif

	return 0;
}

size_t GetHeapMemoryIXray()
{
	size_t free, reserved, commited;
	vminfo(&free, &reserved, &commited);
 	return commited;
}

void CBuild::Run(const char* P)
{
 	lc_global_data()->initialize();

	//****************************************** Open Level
	xr_strconcat(path, P, "\\");
	string_path					lfn;
	IWriter* fs = FS.w_open(xr_strconcat(lfn, path, "level."));
	fs->open_chunk(fsL_HEADER);
	hdrLEVEL H;
	H.XRLC_version = XRCL_PRODUCTION_VERSION;
	H.XRLC_quality = g_params().m_quality;
	fs->w(&H, sizeof(H));
	fs->close_chunk();

	// Dumb entry in shader-registration
	RegisterShader("");

	// Saving lights
	{
		string256			fn;
		IWriter* fs = FS.w_open(xr_strconcat(fn, pBuild->path, "build.lights"));
		fs->w_chunk(0, &*L_static().rgb.begin(), L_static().rgb.size() * sizeof(R_Light));
		fs->w_chunk(1, &*L_static().hemi.begin(), L_static().hemi.size() * sizeof(R_Light));
		fs->w_chunk(2, &*L_static().sun.begin(), L_static().sun.size() * sizeof(R_Light));
		FS.w_close(fs);
	}
	 
	// Optimizing, Adaptive, etc
  	PreOptimize();
	CorrectTJunctions();
 	xrPhase_AdaptiveHT_tessalte();


	Phase("Building (Level, Build).cform ...");
	BuildCForm();
 	mem_Compact();

	// All lighting + lmaps building and saving
	Light();
	RunAfterLight(fs);
}

void CBuild::RunAfterLight(IWriter* fs)
{
	//****************************************** Convert to OGF
	Phase("Converting to OGFs...");
	Flex2OGF();
	mem_Compact();
	//****************************************** Export MU-models
	Phase("Converting MU-models to OGFs...");
	{
		Status("MU : Models...");
		xr_parallel_for(size_t(0), size_t(mu_models().size()), [&] (size_t m)
		{
			calc_ogf(*mu_models()[m]);
		});

		for (u32 m = 0; m < mu_models().size(); m++)
		{
			export_geometry(*mu_models()[m]);
		}
		
		Status("MU : References...");
 		for (u32 m = 0; m < mu_models().size(); m++)
		{
			export_ogf(*mu_refs()[m]);
		}
	}
	mem_Compact();

	Status("MU : References...");
	for (auto mRID = 0; mRID < (mu_refs().size()); mRID++)
	{
		Progress(mRID / mu_refs().size());
		export_ogf(*mu_refs()[mRID]);

		AditionalData("MU : Refference: %u / %u", mRID, mu_refs().size() );
	}
	mem_Compact();


	//****************************************** Build sectors
	Phase("Building sectors...");
	BuildSectors();
	//should be after normals, so that double-sided faces gets separated
	BuildPortals(*fs);
 	mem_Compact();

	//****************************************** Saving MISC stuff
	Phase("Saving...");
	mem_Compact();
	SaveLights(*fs);

	fs->open_chunk(fsL_GLOWS);
 	for (b_glow& G : glows)
	{
		fs->w(&G, 4 * sizeof(float));

		string1024 sid = {};
		xr_strconcat(sid, shader_render[materials()[G.dwMaterial].shader].name, "/", textures()[materials()[G.dwMaterial].surfidx].name);
		fs->w_u16(RegisterShader(sid));
	}
	fs->close_chunk();

	SaveTREE(*fs);
	SaveSectors(*fs);
	// Закрываем запись (а то бывает косяк что процесс закончился но файл не закрыло) 
	FS.w_close(fs);

	err_save();

	clMsg("File is Saved");
	clMsg("Compilation is Ended");
}

void CBuild::err_save()
{
	string_path log_name;
	xr_strconcat(log_name, "build_", Core.UserName, ".err");
	FS.update_path(log_name, "$logs$", log_name);

	IWriter* fs = FS.w_open(log_name);
	IWriter& err = *fs;

	// t-junction
	err.open_chunk(0);
	err.w_u32(err_tjunction().size() / (1 * sizeof(Fvector)));
	err.w(err_tjunction().pointer(), err_tjunction().size());
	err.close_chunk();

	// m-edje
	err.open_chunk(1);
	err.w_u32(err_multiedge().size() / (2 * sizeof(Fvector)));
	err.w(err_multiedge().pointer(), err_multiedge().size());
	err.close_chunk();

	// invalid
	err.open_chunk(2);
	err.w_u32(err_invalid().size() / (3 * sizeof(Fvector)));
	err.w(err_invalid().pointer(), err_invalid().size());
	err.close_chunk();

	FS.w_close(fs);
}

void CBuild::MU_ModelsCalculateNormals()
{
	for (u32 m = 0; m < mu_models().size(); m++)
		calc_normals(*mu_models()[m]);
}

xr_vector<xrMU_Model*>&CBuild::mu_models()
{
	VERIFY(lc_global_data()); 
	return lc_global_data()->mu_models(); 
}

xr_vector<xrMU_Reference*>&CBuild::mu_refs()
{
	VERIFY(lc_global_data()); 
	return lc_global_data()->mu_refs(); 
}

void CBuild::ImplicitLighting()
{
	if (g_params().m_quality == ebqDraft)
		return;

	extern void ImplicitLightingExec();
	ImplicitLightingExec();
}

void CBuild::SaveLights(IWriter& fs)
{
	fs.open_chunk(fsL_LIGHT_DYNAMIC);
	for (b_light_dynamic& L : L_dynamic)
	{
		fs.w_u32(L.controller_ID);
		fs.w(&L.data, sizeof(L.data));
	}
	fs.close_chunk();
}