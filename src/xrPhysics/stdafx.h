
#pragma once

#ifndef XRGAME_EXPORTS
#define	MTL_EXPORT_API
#define ENGINE_API _declspec(dllimport)
#define DLL_API		
#endif
#define ECORE_API

#include "../xrCore/xrCore.h"
#include "../xrCore/API/xrapi.h"
#include "../xrServerEntities/smart_cast.h"

#ifndef smart_cast
#include <fast_dynamic_cast/fast_dynamic_cast.hpp>
#define smart_cast fast_dynamic_cast
#endif

#include "../xrCDB/xrCDB.h"
#include "../xrSound/sound.h"

#include "xrPhysics.h"

class CGameMtlLibrary;
IC CGameMtlLibrary &GMLibrary()
{
	VERIFY(PGMLib);
	return *PGMLib;
}