#include "../../xrCore/xrCore.h"
#define SCRIPTS_API
#include "../xrAI/xrAI.h"

#include "../xrAI/xr_graph_merge.h"
#include "../xrAI/game_spawn_constructor.h"
#include "../xrAI/xrCrossTable.h"
#include "../xrAI/game_graph_builder.h"
#include "../xrAI/spawn_patcher.h"

#include "../xrAI/factory_api.h"
#include "../../xrGame/quadtree.h"

static const char* h_str =
"The following keys are supported / required:\n"
"-? or -h   == this help\n"
"-f<NAME>   == compile level in gamedata/levels/<NAME>/\n"
"-o         == modify build options\n"
"-s         == build game spawn data\n"
"\n"
"NOTE: The last key is required for any functionality\n";

void Help(const char* h_str);

string_path INI_FILE;

extern LPCSTR LEVEL_GRAPH_NAME;

extern LPCSTR GAME_CONFIG;

extern void clear_temp_folder();
extern void	xrCompiler(LPCSTR name, bool draft_mode, bool pure_covers, LPCSTR out_name);
extern void	verify_level_graph(LPCSTR name, bool verbose);

#include "CompilersUI.h"
extern CompilersMode gCompilerMode;

void StartupAI()
{
	// Load project
	string4096 name;
	strcpy(name, gCompilerMode.level_name);

	if (xr_strlen(name))
		xr_strcat(name, "\\");

	string_path			prjName;
	prjName[0] = 0;
	bool				can_use_name = false;
	
	if (xr_strlen(name) < sizeof(string_path))
	{
		can_use_name = true;
		FS.update_path(prjName, "$game_levels$", name);
	}

	FS.update_path(INI_FILE, "$game_config$", GAME_CONFIG);

	if (gCompilerMode.AI_BuildLevel)
	{
		R_ASSERT3(can_use_name, "Too big level name", name);

  		char* output = (pstr) LEVEL_GRAPH_NAME;

		xrCompiler(prjName, gCompilerMode.AI_Draft, gCompilerMode.AI_PureCovers, output);

		
	}
	 
	if (gCompilerMode.AI_Verify)
	{
		R_ASSERT3(can_use_name, "Too big level name", name);
		verify_level_graph(prjName, gCompilerMode.AI_Verbose);
	}



	if (gCompilerMode.AI_Spawn && !gCompilerMode.AI_BuildLevel)
	{
		if (xr_strlen(name))
			name[xr_strlen(name) - 1] = 0;

		char* output = gCompilerMode.AI_spawn_name;
		char* start_level = gCompilerMode.AI_StartActor;

 		clear_temp_folder();
		CGameSpawnConstructor* BuilderSpawn = new CGameSpawnConstructor(name, output, start_level, gCompilerMode.AI_NoSeparatorCheck);
	}
	
}

SEFactory_Create* create_entity = 0;
SEFactory_Destroy* destroy_entity = 0;

static HMODULE hFactory;

void InitialFactory() {
	LPCSTR g_name = "xrSE_Factory.dll";
	Msg("Loading DLL: %s", g_name);
	hFactory = LoadLibraryA(g_name);

	if (0 == hFactory)		
		R_CHK(GetLastError());

	R_ASSERT2(hFactory, "Factory DLL raised exception during loading or there is no factory DLL at all");

#ifdef _M_X64
	create_entity = (SEFactory_Create*)GetProcAddress(hFactory, "create_entity");	R_ASSERT(create_entity);
	destroy_entity = (SEFactory_Destroy*)GetProcAddress(hFactory, "destroy_entity");	R_ASSERT(destroy_entity);
#else
	create_entity = (Factory_Create*)GetProcAddress(hFactory, "_create_entity@4");	R_ASSERT(create_entity);
	destroy_entity = (Factory_Destroy*)GetProcAddress(hFactory, "_destroy_entity@4");	R_ASSERT(destroy_entity);
#endif
}

void DestroyFactory() {
	FreeLibrary(hFactory);
}
 