// Engine.h: interface for the CEngine class.
//
//////////////////////////////////////////////////////////////////////

#pragma once

#include "engineAPI.h"
#include "../xrCore/EventManager.h"
#include "../xrCPU_Pipe/xrCPU_Pipe.h"
#include "xrSheduler.h"

class ENGINE_API CEngine
{
	HMODULE				hPSGP;
public:
	// DLL api stuff
	CEngineAPI			External;
	CSheduler			Sheduler;

	void				Initialize	();
	void				Destroy		();
	
	CEngine();
	~CEngine();
};

ENGINE_API extern xrDispatchTable	PSGP;
ENGINE_API extern CEngine			Engine;

// These variables have corrensponding console commands, and can be used to quickly tune or test smth, without quiting from game
extern ENGINE_API float devfloat1;
extern ENGINE_API float devfloat2;
extern ENGINE_API float devfloat3;
extern ENGINE_API float devfloat4;
