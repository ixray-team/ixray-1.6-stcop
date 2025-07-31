// Build.cpp: implementation of the CBuild class.
//
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"

#include "Build.h"

#include "../xrLC_Light/xrMU_Model.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"
#include "../xrLC_Light/mu_model_light.h"

#include "../xrForms/CompilersUI.h"

//#include "../xrLC_Light/lcnet_task_manager.h"
void	calc_ogf		( xrMU_Model &	mu_model );
void	export_geometry	( xrMU_Model &	mu_model );

void	export_ogf		( xrMU_Reference& mu_reference );

using namespace			std;
struct OGF_Base;
xr_vector<OGF_Base *>	g_tree;

//BOOL					b_noise		= FALSE;
//BOOL					b_radiosity	= FALSE;
//BOOL					b_net_light	= FALSE;
SBuildOptions			g_build_options;
vec2Face				g_XSplit;

void	CBuild::CheckBeforeSave( u32 stage )
{
	bool b_g_tree_empty = g_tree.empty() ;
	R_ASSERT( b_g_tree_empty );
	bool b_g_XSplit_empty = g_XSplit.empty();
	R_ASSERT( b_g_XSplit_empty );
	bool b_IsOGFContainersEmpty = IsOGFContainersEmpty();
	R_ASSERT( b_IsOGFContainersEmpty );
	
	
	
}

void	CBuild::TempSave( u32 stage )
{
	CheckBeforeSave( stage );

}
 


//////////////////////////////////////////////////////////////////////
#include "../xrLC_Light/embree_raytracing/EmbreeRayTrace.h"
CBuild::CBuild()
{
	// Se7kills Initialize Device Embree

}

CBuild::~CBuild()
{
	destroy_global_data();
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

extern u16		RegisterShader		(LPCSTR T);


void CBuild::Light_prepare()
{
	for (vecFaceIt I=lc_global_data()->g_faces().begin();	I!=lc_global_data()->g_faces().end(); I++) (*I)->CacheOpacity();
	for (u32 m=0; m<mu_models().size(); m++)	mu_models()[m]->calc_faceopacity();
}

#include "../xrLC_Light/xrDeflector.h"
#include "../xrLC_Light/Lightmap.h"
#include "../xrLC_Light/xrDeflectorDefs.h"

#include <psapi.h>

size_t last_update_memory = 0;
CTimer tMemory;
size_t GetHeapMemory()
{
	// Не слишком часто обновляться
	if (tMemory.GetElapsed_ms() < 2000)
	{
		return last_update_memory;
	}

	PROCESS_MEMORY_COUNTERS_EX pmc;
	if (GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS*)&pmc, sizeof(pmc)))
	{
		tMemory.Start();
		last_update_memory = pmc.PrivateUsage;
		return pmc.PrivateUsage;
	}

	return 0;
};

size_t GetMemoryUsed()
{
	return GetHeapMemory();
}

void GetMemoryUsedStorage()
{
#ifdef DEBUG
	if (!lc_global_data())
		return;

	u32 tree = pBuild->GetTreeSize() / 1024 / 1024;
	size_t defl = 0; size_t lightmaps = 0;
	for (auto& D : lc_global_data()->g_deflectors())
	{
		defl += D->size_deflector();
	}
	defl /= (1024 * 1024);
 	for (auto&  LM  : lc_global_data()->lightmaps())
	{
		lightmaps += LM->lm.memory_lmap();
 	}
	lightmaps /= (1024 * 1024);
  	 
	size_t sV = 0;
	for (auto& V : lc_global_data()->g_vertices())
	{
		sizeof(Vertex);
		sV += V->used_memory(); 
	}
	
	sV /= (1024 * 1024);

	size_t sF = 0;// lc_global_data()->g_faces().size() * sizeof(Face);
	for (auto& F : lc_global_data()->g_faces())
	{						
		sF += sizeof(*F) + sizeof(void*); //void* vector size
	}
	
	sF /= (1024 * 1024);
	
	size_t TAlloc = 0;
	for (auto& T : lc_global_data()->textures())
	{
		TAlloc += (T.dwHeight* T.dwHeight * 4);
	}
	lc_global_data()->textures();
	size_t sTex = lc_global_data()->textures().size() * sizeof(b_BuildTexture);
	sTex += TAlloc;
	sTex /= (1024 * 1024);

	size_t SplitsMemory = 0;
	for (auto X : g_XSplit)
	{
		SplitsMemory += X->capacity() * sizeof(void*);
	}
	SplitsMemory += g_XSplit.capacity() * sizeof(void*);

	u32 MB = 1024 * 1024;

	Msg("!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
	Msg("- xSplits:		%u count, %u capacity, %u mb", g_XSplit.size(), g_XSplit.capacity(), SplitsMemory / 1024 / 1024);
	
	Msg("- GeomTree:	%u mb, cap:%u",		tree, g_tree.capacity());
	Msg("- Deflectors:	%u mb, cap:%u",		defl, lc_global_data()->g_deflectors().capacity());
	Msg("- Lightmaps:	%u mb, cap:%u",		lightmaps, lc_global_data()->lightmaps().capacity());
 	Msg("- vertexes:	%u mb, store: %u, cap:%u",		sV, lc_global_data()->g_vertices().size(),  lc_global_data()->g_vertices().capacity());	// Не учитываю алокацию adjucement при создании и жрет в разы больше
	Msg("- faces:		%u mb, store: %u, cap:%u",		sF, lc_global_data()->g_faces().size(), lc_global_data()->g_faces().capacity());	// Не учитываю алокацию adjucement при создании и жрет в разы больше
	Msg("- Textures:	%u mb, cap:%u",		sTex, lc_global_data()->textures().capacity());
	Msg("- Embree BVH: %umb, Static: %umb, MU: %umb", EmbreeMain.BVH_size / MB, EmbreeMain.Static_size / MB, EmbreeMain.MU_size / MB);

	u32 memdata = tree + defl + lightmaps;
	
	u32 EmbreeMem = (EmbreeMain.BVH_size / MB) + (EmbreeMain.Static_size / MB) + (EmbreeMain.MU_size / MB);
 
	if (g_XSplit.capacity() != g_XSplit.size())
		g_XSplit.shrink_to_fit();

	if (lc_global_data()->g_deflectors().capacity() != lc_global_data()->g_deflectors().size())
		lc_global_data()->g_deflectors().shrink_to_fit();

	if (lc_global_data()->lightmaps().capacity() != lc_global_data()->lightmaps().size())
		lc_global_data()->lightmaps().shrink_to_fit();

	Msg("- Total Memory (AFTER SHIRK):	%u / (Check) %u + %u + %u mb", GetHeapMemory() / (1024 * 1024), EmbreeMem, sV + sF + sTex, memdata);


	Msg("!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
#endif
}

void CBuild::Run	(LPCSTR P)
{
	lc_global_data()->initialize();
 
	//****************************************** Open Level
	xr_strconcat(path,P,"\\")	;
	string_path					lfn				;
	IWriter* fs					= FS.w_open		(xr_strconcat(lfn,path,"level."));
	fs->open_chunk				(fsL_HEADER)	;
	hdrLEVEL H;	
	H.XRLC_version				= XRCL_PRODUCTION_VERSION;
	H.XRLC_quality				= g_params().m_quality;
	fs->w						(&H,sizeof(H));
	fs->close_chunk				();

	//****************************************** Dumb entry in shader-registration
	RegisterShader				("");

	//****************************************** Saving lights
	{
		string256			fn;
		IWriter*		fs	= FS.w_open	(xr_strconcat(fn,pBuild->path,"build.lights"));
		fs->w_chunk			(0,&*L_static().rgb.begin(),L_static().rgb.size()*sizeof(R_Light));
		fs->w_chunk			(1,&*L_static().hemi.begin(),L_static().hemi.size()*sizeof(R_Light));
		fs->w_chunk			(2,&*L_static().sun.begin(),L_static().sun.size()*sizeof(R_Light));
		FS.w_close			(fs);
	}
	 
	//****************************************** Optimizing + checking for T-junctions
	mem_Compact();


	Phase("Optimizing...");
 	PreOptimize();
	CorrectTJunctions();
	mem_Compact();

	// AdaptiveHT
  	BuildAdaptiveHT();

	//****************************************** Building normals
	Phase("Building normals...");
	mem_Compact();
	CalcNormals();

	//****************************************** Collision DB
	//should be after normals, so that double-sided faces gets separated
	FPU::m64r					();
	Phase						("Building collision database (CFORM)...");
	mem_Compact					();


 	if (!gCompilerMode.LC_BackingDisabled)
		BuildCForm					();
	BuildPortals				(*fs);


	//****************************************** GLOBAL-ILLUMINATION
	if (g_build_options.b_radiosity)			
	{
		FPU::m64r					();
		Phase						("Radiosity-Solver...");
		mem_Compact					();
		Light_prepare				();
		xrPhase_Radiosity			();
	}
	//****************************************** All lighting + lmaps building and saving
	 

	Light						();
	RunAfterLight				( fs );
}
void	CBuild::StartMu	()
{
  //mu_base.start				(new CMUThread (0));
  run_mu_light(  );
}


void CBuild::	RunAfterLight			( IWriter* fs	)
{
	//****************************************** T-Basis
	{
		FPU::m64r();
		Phase("Building tangent-basis...");
		xrPhase_TangentBasis();
		mem_Compact();
	}


  	//****************************************** Convert to OGF
	FPU::m64r					();
	Phase						("Converting to OGFs...");
	mem_Compact					();
	Flex2OGF					();

	//****************************************** Export MU-models
	FPU::m64r					();
	Phase						("Converting MU-models to OGFs...");
	mem_Compact					();
	{
		u32 m;
		Status			("MU : Models...");
		for (m=0; m<mu_models().size(); m++)	{
			calc_ogf			(*mu_models()[m]);
			export_geometry		(*mu_models()[m]);
		}

		Status			("MU : References...");
		for (m=0; m<mu_refs().size(); m++)
			export_ogf(*mu_refs()[m]);
  	}

	Status			("MU : References...");
	xr_atomic_u32 index = 0; 

	for (auto mRID = 0; mRID < (mu_refs().size()); mRID++)
 	{
		Progress(mRID / mu_refs().size());
		export_ogf(*mu_refs()[mRID]);
 		// if (index.load() % 1024 == 0)
		// 	clMsg("[MT] Export MUOgf: %u/%u", mRID, mu_refs().size());
		index.fetch_add(1);
	}

 
	//****************************************** Build sectors
	FPU::m64r		();
	Phase			("Building sectors...");
	mem_Compact		();
	BuildSectors	();

	//****************************************** Saving MISC stuff
	FPU::m64r		();
	Phase			("Saving...");
	mem_Compact		();
	SaveLights		(*fs);

	fs->open_chunk	(fsL_GLOWS);
	
	for (u32 i=0; i<glows.size(); i++)
	{
		b_glow&	G	= glows[i];
		fs->w		(&G,4*sizeof(float));
		string1024	sid;
		xr_strconcat(sid,
			shader_render[materials()[G.dwMaterial].shader].name,
			"/",
			textures()		[materials()[G.dwMaterial].surfidx].name
			);
		fs->w_u16	(RegisterShader(sid));
	}
	fs->close_chunk	();

	SaveTREE		(*fs);
	SaveSectors		(*fs);
	// Закрываем запись (а то бывает косяк что процесс закончился но файл не закрыло) 
	FS.w_close(fs);

	err_save		();



	clMsg("File is Saved");

	clMsg("Compilation is Ended");

	

}

void CBuild::err_save	()
{
	string_path		log_name;
	xr_strconcat(log_name,"build_",Core.UserName,".err");
	FS.update_path	(log_name,"$logs$",log_name);

	IWriter*		fs	= FS.w_open(log_name);
	IWriter&		err = *fs;

	// t-junction
	err.open_chunk	(0);
	err.w_u32		(err_tjunction().size()/(1*sizeof(Fvector)));
	err.w			(err_tjunction().pointer(), err_tjunction().size());
	err.close_chunk	();

	// m-edje
	err.open_chunk	(1);
	err.w_u32		(err_multiedge().size()/(2*sizeof(Fvector)));
	err.w			(err_multiedge().pointer(), err_multiedge().size());
	err.close_chunk	();

	// invalid
	err.open_chunk	(2);
	err.w_u32		(err_invalid().size()/(3*sizeof(Fvector)));
	err.w			(err_invalid().pointer(), err_invalid().size());
	err.close_chunk	();

	FS.w_close( fs );
}

void CBuild::MU_ModelsCalculateNormals()
{
	for		(u32 m=0; m<mu_models().size(); m++)
		calc_normals( *mu_models()[m] );
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
	::ImplicitLighting( );
}