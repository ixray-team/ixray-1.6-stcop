#include "../xrLC/StdAfx.h"
#include "../xrLC/Build.h"
#include "../xrLC_Light/xrLC_GlobalData.h"

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
"-skip_weld == don't use welding vertexis \n"
"-lmaps_alt == Используем альтернативный метод ligtmaps building texture \n"
"-lmaps_size == Менять размер тексуры LMAP\n"
"-use_intel == Используем альтернативный метод Raytracing Intel Embree \n"

"\n"
"NOTE: The last key is required for any functionality\n";

void Help(const char*);

typedef int __cdecl xrOptions(b_params* params, u32 version, bool bRunBuild);
extern bool g_using_smooth_groups;

void StartupLC(LPSTR lpCmdLine) 
{
	create_global_data();
	char cmd[512], name[256];

	xr_strcpy(cmd, lpCmdLine);
	_strlwr(cmd);
	if (strstr(cmd, "-?") || strstr(cmd, "-h")) { Help(h_str); return; }
	if (strstr(cmd, "-f") == 0) { Help(h_str); return; }
	if (strstr(cmd, "-gi"))								g_build_options.b_radiosity = TRUE;
	if (strstr(cmd, "-noise"))							g_build_options.b_noise = TRUE;
 	if (strstr(Core.Params, "-nosmg"))					g_using_smooth_groups = false;

	VERIFY(lc_global_data());
	lc_global_data()->b_nosun_set(!!strstr(cmd, "-nosun"));
	lc_global_data()->SetSkipInvalid(strstr(cmd, "-skipinvalid") != nullptr);
	lc_global_data()->SetSkipTesselate(strstr(cmd, "-notess") != nullptr);
	lc_global_data()->SetLmapRGBA(strstr(cmd, "-tex_rgba") != nullptr);
	lc_global_data()->SetSkipSubdivide(strstr(cmd, "-nosubd") != nullptr);
	
	// Se7kills
	lc_global_data()->SetIsIntelUse(strstr(cmd, "-use_intel") != nullptr);
	lc_global_data()->SetSkipWeld(strstr(cmd, "-skip_weld") != nullptr);
	
	if (strstr(cmd, "-lmaps_1k") != nullptr)
		lc_global_data()->SetLmapsSize(1024);
	else 
	if (strstr(cmd, "-lmaps_2k") != nullptr)
		lc_global_data()->SetLmapsSize(1024 * 2);
	else 
	if (strstr(cmd, "-lmaps_4k") != nullptr)
		lc_global_data()->SetLmapsSize(1024 * 4);
	else
	if (strstr(cmd, "-lmaps_8k") != nullptr)
		lc_global_data()->SetLmapsSize(1024 * 8);
	else
		lc_global_data()->SetLmapsSize(1024 * 4); // 4k Default


	// Faster FPU 
	SetPriorityClass(GetCurrentProcess(), NORMAL_PRIORITY_CLASS);

	// Load project
	name[0] = 0;
	sscanf(strstr(cmd, "-f") + 2, "%s", name);

	extern  HWND logWindow;
	string256				temp;
	xr_sprintf(temp, "%s - Levels Compiler", name);
	SetWindowTextA(logWindow, temp);

	string_path prjName;
	FS.update_path(prjName, "$game_levels$", xr_strconcat(prjName, name, "\\build.prj"));

	string256 phaseName;
	Phase(xr_strconcat(phaseName, "Reading project [", name, "]..."));

	string256 inf;
	IReader* F = FS.r_open(prjName);
	if (NULL == F) {
		xr_sprintf(inf, "Build failed!\nCan't find level: '%s'", name);
		clMsg(inf);
		MessageBoxA(logWindow, inf, "Error!", MB_OK | MB_ICONERROR);
		return;
	}

	// Version
	F->r_chunk(EB_Version, &version);
	clMsg("version: %d", version);
	R_ASSERT(XRCL_CURRENT_VERSION == version);

	// Header
	b_params Params;
	F->r_chunk(EB_Parameters, &Params);

	// Conversion
	Phase("Converting data structures...");
	pBuild = new CBuild();
	pBuild->Load(Params, *F);
	FS.r_close(F);

	// Call for builder
	string_path lfn;
	FS.update_path(lfn, _game_levels_, name);
	pBuild->Run(lfn);
	xr_delete(pBuild);
}
