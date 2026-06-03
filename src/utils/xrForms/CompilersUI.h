#pragma once
#include "../../xrEngine/xrLevel.h"

struct ImFont;

struct LevelFileData
{
	xr_string Name;
	bool Select = false;
};

enum class LCLightmapFormat
{
	FORMAT_RGBA,
	FORMAT_BC7,
	FORMAT_BC5
};

enum class LCBuildingType
{
	eNone = -1,
	eLC = 0,
	eAI = 1, 
	eDO = 2
};

struct CompilersMode
{
 	LCBuildingType builder_type = LCBuildingType::eNone;
	LCLightmapFormat LmapsFormat = LCLightmapFormat::FORMAT_RGBA;

	bool Silent = false;
	bool Embree = true;
	bool CUDA = true;

 	bool EmbreeBVHCompact	= false; // ������ Traversing
	bool EmbreeBVHRobust	= false; // ������ Traversing
	bool EmbreeRays8		= true;  // x2 Speed
	bool EmbreeInstaces     = false; // ������ Traversing

	bool ClearTemp = false;
	bool SkipTHM = false;

	bool AI = false;
	bool DO = false;
	bool LC = false;

	// Geometry Optimizers
	bool LC_OGF_PROGRESSIVE = true;
	bool LC_OGF_STRIPTIFY   = true;
	bool LC_OGF_TANGENT		= true;

	// LC Geometry Stuff
	bool LC_SkipInvalidFaces = true;
	bool LC_NoSMG = true;
 	bool LC_Tess = true;
 	bool LC_skipWeld = false;
	 
	// Geometry Export
	GeomVanillaType LC_GeomType = GeomVanillaType::Vanilla;
	CFormVersions LC_CformType = CFormVersions::Vanilla;
	int LC_CFormChunkSize = 75;
	int LC_GeomChunkSize = 75;
	 
	// Lightmaps
 	int  LC_BORDER = 1;
	int  LC_sizeLmaps = 1024 * 4;
	bool LC_legacyLM = false;
	bool LC_SkipStaticMap = false;
	bool LC_fast_way = false;
	bool LC_NoSun = false;

	// Settings Owerride
	bool IsOverloadedSettings = false;
	int LC_JSampleMU = 6;
	int LC_JSample = 9;
	float WeldDistance = 0.005f;
	float LC_Pixels = 10;
 
	// SPAWN COMPILER
	bool AI_BuildSpawn = false;
	char AI_spawn_name[256];
	char AI_StartActor[256];
	bool AI_NoSeparatorCheck = true;
	bool AI_FreeMPBuild = false;

	bool AI_BuildLevel = false;
	bool AI_PureCovers = false;
	bool AI_Draft = false;
	bool AI_Verify = false; 
	bool AI_Verbose = false;

	xr_vector<LevelFileData> Files;
	ImFont* CompilerIconsFont;

	// ������� �������
	xr_string compilation_level;
	str_c get_lname()
	{
		return compilation_level.c_str();
	}

	// ������ !
	int ThreadsPerWork = 14;
};

void RenderMainUI();
void RenderCompilerUI(int X, int Y);
void InitializeUIData();
void SaveCompilerCfg();

extern CompilersMode gCompilerMode;;