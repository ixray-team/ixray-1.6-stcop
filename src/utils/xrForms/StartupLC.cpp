#include "../xrLC/StdAfx.h"
#include "../xrLC/Build.h"
#include "../xrLC_Light/xrLC_GlobalData.h"
 
#include <CompilersUI.h>

CBuild* pBuild = NULL;
u32		version = 0;

static const char* h_str =
"The following keys are supported / required:\n"
"-? or -h	== this help\n"
"-o			== modify build options\n"
"-nosun		== disable sun-lighting\n"
"-skipinvalid\t== skip crash if invalid faces exists\n"
"-notess	== don`t use tesselate geometry\n"
"-nosubd	== don`t use subdivide geometry\n"
"-tex_rgba	== don`t compress lightmap textures\n"
"-f<NAME>	== compile level in GameData\\Levels\\<NAME>\\\n"
"\n"
"NOTE: The last key is required for any functionality\n";

void Help(const char*);

typedef int __cdecl xrOptions(b_params* params, u32 version, bool bRunBuild);
extern bool g_using_smooth_groups;

extern CompilersMode gCompilerMode;

void StartupLC() 
{
 	g_using_smooth_groups			= !gCompilerMode.LC_NoSMG;

	// Load project
	for (auto& [Name, Selected] : gCompilerMode.Files)
	{
		if (!Selected)	continue;

		gCompilerMode.compilation_level = Name;
 		create_global_data();
	
		// Se7kills
		string256 temp;
		xr_sprintf(temp, "%s - Levels Compiler", Name.data());
		SDL_SetWindowTitle(g_AppInfo.Window, temp);

		string_path prjName;
		FS.update_path(prjName, "$game_levels$", xr_strconcat(prjName, Name.data(), "\\build.prj"));

  		string256 inf;
		IReader* F = FS.r_open(prjName);
		if (NULL == F)
		{
			xr_sprintf(inf, "Build failed!\n! Can't find level: '%s'", Name.data());
			clMsg(inf);
			MessageBoxA(nullptr, inf, "Error!", MB_OK | MB_ICONERROR);
			return;
		}

		// Version
		F->r_chunk(EB_Version, &version);
 		R_ASSERT(XRCL_CURRENT_VERSION == version);

		// Header
		b_params Params;
		F->r_chunk(EB_Parameters, &Params);

		// Conversion
		pBuild = new CBuild();
		
		Phase("Converting data structures...");
 		pBuild->Load(Params, *F);
		FS.r_close(F);

		// Test
		if (gCompilerMode.IsOverloadedSettings)
		{
			g_params().m_lm_jitter_samples = gCompilerMode.LC_JSample;
			g_params().m_lm_pixels_per_meter = gCompilerMode.LC_Pixels;
  			g_params().m_weld_distance = gCompilerMode.WeldDistance;
		}

		// Call for builder
		string_path lfn;
		FS.update_path(lfn, _game_levels_, Name.data());
		pBuild->Run(lfn);
		xr_delete(pBuild);

		PhaseEnd();
	}
	    
	 
}
