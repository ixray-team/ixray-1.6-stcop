// EngineAPI.h: interface for the CEngineAPI class.
//
//****************************************************************************
// Support for extension DLLs
//****************************************************************************
#pragma once
#include <array>
#include "../xrRHI/RHIEnums.h"

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
	ScenesViewer,
	CameraEffectors,
	Game_TimeManager,
	Game_SpawnManager,
	Game_WeaponManager,
	Game_SearchManager,
	Game_HudAdjustManager,
	Tools_RenderDebug_SVGStorageViewer,
	Tools_OMFEditor,
	Tools_InputManager,
	Tools_CarEditor,
	Tools_PostProcessEffectorEditor,
	Tools_TextureEditor,
	UI_General,
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
