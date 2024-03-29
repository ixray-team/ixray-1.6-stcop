////////////////////////////////////////////////////////////////////////////
//	Module 		: guid_generator.h
//	Created 	: 21.03.2005
//  Modified 	: 21.03.2005
//	Author		: Dmitriy Iassenev
//	Description : GUID generator
////////////////////////////////////////////////////////////////////////////
#pragma once

#include "../../xrEngine/xrLevel.h"

#ifdef AI_COMPILER
#define GUID_API_INTERNAL
#else
#define GUID_API_INTERNAL ENGINE_API
#endif

GUID_API_INTERNAL extern xrGUID generate_guid();
GUID_API_INTERNAL extern LPCSTR generate_guid(const xrGUID &guid, LPSTR buffer, const u32 &buffer_size);
