#pragma once
#include "xrLevel.h"
#include "xrCore.h"

struct ImFont;

namespace CFormConverter
{
	struct LevelFileData
	{
		xr_string Name;
		bool Select = false;
	};

	struct ConverterSettings
	{
		int LC_GeomChunkSize = 75;
		CFormVersions LC_CformType = CFormVersions::Vanilla;
		int LC_CFormChunkSize = 75;
		int LC_sizeLmaps = 1024 * 4;
		int ThreadsPerWork = 14;
		GeomVanillaType LC_GeomType = GeomVanillaType::Vanilla;

		bool Geom;
		bool CForm;
		bool Spawn;

		xr_vector<LevelFileData> Files;
		ImFont* CompilerIconsFont;
		
		xr_stack_string256 SpawnOrig = "all";
		xr_stack_string256 SpawnDest = "all_conv";
	};

	inline ConverterSettings& GetConverterSettings()
	{
		static ConverterSettings settings;
		return settings;
	}

	void RenderMainUI();
	void RenderCompilerUI(int X, int Y);
	void InitializeUIData();
	void SaveCompilerCfg();
	void StartCompile();
}