// EngineAPI.h: interface for the CEngineAPI class.
//
//****************************************************************************
// Support for extension DLLs
//****************************************************************************
#pragma once
#include <array>

enum class APILevel;

// Abstract 'Pure' class for DLL interface
class ENGINE_API DLL_Pure {
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

enum class EditorUI : std::uint8_t
{
	Shaders,
	Weather,
	DebugDraw,
	CmdVars,
	CmdConsole,
	Statistics,
	Profiler,
	LuaDebug,

	Count
};

class ENGINE_API		CEngineAPI
{
private:
	HMODULE				hGame;
	HMODULE				hRender;

public:
	HMODULE				hGameSpy;

	std::array<bool, static_cast<std::uint8_t>(EditorUI::Count)> EditorStates = {};

public:
	Factory_Create*		pCreate;
	Factory_Destroy*	pDestroy;
	void				Initialize	();
	
	void				InitializeNotDedicated();
	void				Destroy		();

	void				CreateRendererList();

	APILevel			GetAPI();

	CEngineAPI	();
	~CEngineAPI	();
};

#define NEW_INSTANCE(a)		Engine.External.pCreate(a)
#define DEL_INSTANCE(a)		{ Engine.External.pDestroy(a); a=nullptr; }
