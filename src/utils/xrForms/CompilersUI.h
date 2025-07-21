#pragma once

struct CompilersMode
{
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

	bool DO_NoSun = false;

	bool AI_NoSeparatorCheck = true;
	bool AI_Draft = false;
	bool AI_BuildLevel = true;
	bool AI_Spawn = false;
};

void RenderMainUI();
void InitializeUIData();