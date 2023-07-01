// Engine.h: interface for the CEngine class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_ENGINE_H__22802DD7_D7EB_4234_9781_E237657471AC__INCLUDED_)
#define AFX_ENGINE_H__22802DD7_D7EB_4234_9781_E237657471AC__INCLUDED_
#pragma once

#include "engineAPI.h"
#include "eventAPI.h"
#include "../xrCPU_Pipe/xrCPU_Pipe.h"
#include "xrSheduler.h"

class ENGINE_API CEngine
{
	HMODULE				hPSGP;
public:
	// DLL api stuff
	CEngineAPI			External;
	CEventAPI			Event;
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

#endif // !defined(AFX_ENGINE_H__22802DD7_D7EB_4234_9781_E237657471AC__INCLUDED_)
