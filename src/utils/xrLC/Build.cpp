// Build.cpp: implementation of the CBuild class.
//
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"

#include "Build.h"

#include "../xrLC_Light/xrMU_Model.h"



#include "../xrLC_Light/xrLC_GlobalData.h"
#include "../xrLC_Light/xrFace.h"
#include "../xrLC_Light/mu_model_light.h"

//#include "../xrLC_Light/lcnet_task_manager.h"
void	calc_ogf		( xrMU_Model &	mu_model );
void	export_geometry	( xrMU_Model &	mu_model );

void	export_ogf		( xrMU_Reference& mu_reference );

using namespace			std;
struct OGF_Base;
xr_vector<OGF_Base *>	g_tree;

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

CBuild::CBuild()
{
	

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


//.#define CFORM_ONLY
#ifdef LOAD_GL_DATA
void net_light ();
#endif
void CBuild::Run	(LPCSTR P)
{
	lc_global_data()->initialize();
#ifdef LOAD_GL_DATA
	net_light ();
	return;
#endif
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
	FPU::m64r					();
	Phase						("Optimizing...");
	mem_Compact					();
	PreOptimize					();
	CorrectTJunctions			();

	//****************************************** HEMI-Tesselate
	FPU::m64r					();
	Phase						("Adaptive HT...");
	mem_Compact					();
#ifndef CFORM_ONLY
	xrPhase_AdaptiveHT			();
#endif

	//****************************************** Building normals
	FPU::m64r					();
	Phase						("Building normals...");
	mem_Compact					();
	CalcNormals					();
	//SmoothVertColors			(5);

	//****************************************** Collision DB
	//should be after normals, so that double-sided faces gets separated
	FPU::m64r					();
	Phase						("Building collision database...");
	mem_Compact					();
	BuildCForm					();

#ifdef CFORM_ONLY
	return;
#endif

	BuildPortals				(*fs);

	//****************************************** T-Basis
	{
		FPU::m64r					();
		Phase						("Building tangent-basis...");
		xrPhase_TangentBasis		();
		mem_Compact					();
	}

	//****************************************** GLOBAL-RayCast model
	FPU::m64r					();
	Phase						("Building rcast-CFORM model...");
	mem_Compact					();
	Light_prepare				();
	BuildRapid					(TRUE);

	//****************************************** GLOBAL-ILLUMINATION
	if (g_build_options.b_radiosity)			
	{
		FPU::m64r					();
		Phase						("Radiosity-Solver...");
		mem_Compact					();
		Light_prepare				();
		xrPhase_Radiosity			();
	}

	//****************************************** Resolve materials
	FPU::m64r					();
	Phase						("Resolving materials...");
	mem_Compact					();
	xrPhase_ResolveMaterials	();
	IsolateVertices				(TRUE);

	//****************************************** UV mapping
	{
		FPU::m64r					();
		Phase						("Build UV mapping...");
		mem_Compact					();
		xrPhase_UVmap				();
		IsolateVertices				(TRUE);
	}
	
	//****************************************** Subdivide geometry
	if (!lc_global_data()->GetSkipSubdivide())
	{
		FPU::m64r();
		Phase("Subdividing geometry...");
		mem_Compact();
		xrPhase_Subdivide();
		//IsolateVertices(TRUE);
		lc_global_data()->vertices_isolate_and_pool_reload();
	}

	//****************************************** All lighting + lmaps building and saving
	 

	Light						();

 	//****************************************** Starting MU
	FPU::m64r();
	Phase("LIGHT: Starting MU...");
	mem_Compact();
	Light_prepare();
	StartMu();

	RunAfterLight				( fs );
}

void	CBuild::StartMu	()
{
  //mu_base.start				(new CMUThread (0));
  run_mu_light(  );
}

void CBuild::	RunAfterLight			( IWriter* fs	)
{
 	//****************************************** Merge geometry
	FPU::m64r					();
	Phase						("Merging geometry...");
	mem_Compact					();
	xrPhase_MergeGeometry		();

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

//		lc_global_data()->clear_mu_models();
	}

	//****************************************** Destroy RCast-model
	FPU::m64r		();
	Phase			("Destroying ray-trace model...");
	mem_Compact		();
	lc_global_data()->destroy_rcmodel();

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

	err_save		();

	// Закрываем запись (а то бывает косяк что процесс закончился но файл не закрыло) 
	FS.w_close(fs);
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