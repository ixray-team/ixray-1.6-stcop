#pragma once

struct ImFont;

struct LevelFileData
{
	xr_string Name;
	bool Select = false;
};

struct CompilersMode
{
	bool Silent = false;
	bool Embree = true;

	bool Embree_SplitBVH = false;
	bool EmbreeBVHCompact = false;
	bool EmbreeBVHRobust = false;
 	bool ClearTemp = false;
	bool SkipTHM = false;

	bool AI = false;
	bool DO = false;
	bool LC = false;

	bool LC_SaveOFG = false;
	bool LC_GI = false;
	bool LC_NoSun = false;
	bool LC_NoSMG = true;
	bool LC_Noise = false;
	bool LC_Tess = true;
	bool LC_SkipInvalidFaces = true;
	bool LC_tex_rgba = false;
	bool LC_NoSubdivide = false;
	bool LC_skipWeld = false;
 
	int LC_sizeLmaps = 1024 * 4;

	bool IsOverloadedSettings = false;
	int LC_JSampleMU = 6;
	int LC_JSample = 9;
	int ThreadsPerWork = 14;
	
	float LC_Pixels  = 10;

	float WeldDistance = 0.005f;

 
	bool DO_NoSun = false;

	// SPAWN COMPILER
	bool AI_BuildSpawn = false;
	char AI_spawn_name[256];
	char AI_StartActor[256];
	bool AI_NoSeparatorCheck = true;


	bool AI_BuildLevel = false;
	bool AI_PureCovers = false;
	bool AI_Draft = false;
	bool AI_Verify = false; 
	bool AI_Verbose = false;

	xr_vector<LevelFileData> Files;
	ImFont* CompilerIconsFont;
};

void RenderMainUI();
void RenderCompilerUI(int X, int Y);
void InitializeUIData();