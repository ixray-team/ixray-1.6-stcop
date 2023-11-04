// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#pragma once
#include "../xrLC_Light/xrLC_Light.h"


#undef ENGINE_API
#define ENGINE_API				// fake, to enable sharing with engine
//comment - ne figa oni ne sharyatsya

#undef ECORE_API
#define ECORE_API				// fake, to enable sharing with editors
#define XR_EPROPS_API
#include "../../xrcore/clsid.h"
#include "defines.h"
#include "../xrForms/cl_log.h"

#include "b_globals.h"