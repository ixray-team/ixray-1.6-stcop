
#pragma once

#ifndef XRGAME_EXPORTS
#define	MTL_EXPORT_API
#define ENGINE_API _declspec(dllimport)
#define DLL_API		
#endif
#define ECORE_API

#include "../xrCore/xrCore.h"
#include "../xrCore/API/xrAPI.h"

#include "../xrCore/Collision/xrCDB.h"
#include "../xrSound/Sound.h"

#include "xrPhysics.h"

class CGameMtlLibrary;
IC CGameMtlLibrary &GMLibrary()
{
	VERIFY(PGMLib);
	return *PGMLib;
}