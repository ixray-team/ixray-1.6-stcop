#pragma once

struct CompilersMode
{
	char level_name[128];
	

	bool Silent = false;
	bool Embree = true;
	bool ClearTemp = false;

	bool AI = false;
	bool DO = false;
	bool LC = false;

	bool LC_GI = false;
	bool LC_NoSun = false;
	bool LC_NoSMG = false;
	bool LC_Noise = false;
	bool LC_Tess = true;
	bool LC_SkipInvalidFaces = true;
	bool LC_tex_rgba = false;
	bool LC_NoSubdivide = false;
	bool LC_skipWeld = false;

	int LC_sizeLmaps = 1024 * 4;

	int LC_JSampleMU = 6;
	int LC_JSample = 9;
	float LC_Pixels  = 10;
 
	bool DO_NoSun = false;

	// SPAWN COMPILER
	bool AI_Spawn = false;
	char AI_spawn_name[128];
	char AI_StartActor[128];
	bool AI_NoSeparatorCheck = true;


	bool AI_BuildLevel = true;
	bool AI_PureCovers = false;
	bool AI_Draft = false;
	bool AI_Verify = false; 
	bool AI_Verbose = false;
};

void RenderMainUI();
void InitializeUIData();