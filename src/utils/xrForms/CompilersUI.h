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

	bool AI_NoSeparatorCheck = true;
	bool AI_Draft = false;
	bool AI_BuildLevel = true;
	bool AI_Spawn = false;
};

void RenderMainUI();
void InitializeUIData();