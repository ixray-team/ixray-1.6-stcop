// EngineAPI.h: interface for the CEngineAPI class.
//
//****************************************************************************
// Support for extension DLLs
//****************************************************************************
#pragma once
#include <array>
#include "../xrRHI/RHIEnums.h"

enum class ELightingMode : u8
{
	Static = 0,
	Dynamic = 1,
};

ENGINE_API extern ELightingMode g_lighting_mode;
ENGINE_API extern ELightingMode g_lighting_mode_cfg;
ENGINE_API extern bool g_lighting_mode_locked;
ENGINE_API extern int g_current_renderer;

IC bool LightingModeIsStatic() { return g_lighting_mode == ELightingMode::Static; }
IC bool LightingModeIsDynamic() { return g_lighting_mode == ELightingMode::Dynamic; }
// Renderer-facing lighting mode: editors always render the deferred path, so they count as dynamic
ENGINE_API bool LightingModeIsStaticRender();
ENGINE_API bool LightingModeIsDynamicRender();

ENGINE_API bool LightingModeParseToken(const char* name, ELightingMode& mode);
ENGINE_API const char* LightingModeCanonicalToken(ELightingMode mode);
ENGINE_API void LightingModeApply(ELightingMode mode, bool commit_active);
ENGINE_API void LightingModeLockActive();

// Abstract 'Pure' class for DLL interface
class ENGINE_API DLL_Pure
{
public:
	CLASS_ID				CLS_ID;

	DLL_Pure(void *params)	{CLS_ID=0; };
	DLL_Pure()				{CLS_ID=0; };
	virtual	DLL_Pure*		_construct		()	{ return this; 	}
	virtual ~DLL_Pure()		{};
};

// Class creation/destroying interface
extern "C" 
{
typedef  DLL_Pure*	  __cdecl Factory_Create	(CLASS_ID	CLS_ID);
typedef  void		  __cdecl Factory_Destroy	(DLL_Pure*	O);
};

enum class EditorUI : u8
{
	Shaders,
	Weather,
	DebugDraw,
	CmdVars,
	CmdConsole,
	LuaDebug,
	LuaCodespace,
	ActorInfos,
	HudAdjust,
	LevelInspector,
	CutsceneAdjust,
	ScenesViewer,
	CameraEffectors,
	Game_TimeManager,
	Game_SpawnManager,
	Game_WeaponManager,
	Game_SearchManager,
	Game_GraphEditor,
	Game_HudAdjustManager,
	Game_WatchDebug,
	Game_3rdAdjust,
	Game_3DIconAdjust,
	Game_DemoRecord,
	Game_ActorAnimations,
	Game_CinemaFilmMakerEditor,
	Tools_RenderDebug_SVGStorageViewer,
	Tools_DetailLayersEditor,
	Tools_OMFEditor,
	Tools_InputManager,
	Tools_CarEditor,
	Tools_PostProcessEffectorEditor,
	Tools_TextureEditor,
	Tools_QuestEditor,
	Audio_General,
	UI_General,
	ECSViewer,
	Tools_AIGridEditor,
	Profiler,
	Count
};

class ENGINE_API CEngineAPI
{
private:
	HMODULE hRender;

public:
	HMODULE hGame;
	HMODULE hGameSpy;

	Factory_Create* pCreate;
	Factory_Destroy* pDestroy;

	std::array<bool, static_cast<u8>(EditorUI::Count)> EditorStates = {};

public:
	CEngineAPI();
	~CEngineAPI();

public:
	void				Initialize	();
	void				InitializeNotDedicated();
	void				InitializeDedicated();
	void				Destroy		();

	void				CreateRendererList();
	ERHI_API_LAYER		GetAPI();
	int					GetSkinningMode() const;
	void				SetSkinningMode(int Mode = -1);
};

#define NEW_INSTANCE(a)		Engine.External.pCreate(a)
#define DEL_INSTANCE(a)		{ Engine.External.pDestroy(a); a=NULL; }
