#pragma once

#pragma warning( 4 : 4018 )
#pragma warning( 4 : 4244 )
#pragma warning(disable:4505)

#if XRAY_EXCEPTIONS
#	define	THROW(expr)				do {if (!(expr)) {string4096	assertion_info; ::Debug.gather_info(_TRE(#expr),   0,   0,0,DEBUG_INFO,assertion_info); throw assertion_info;}} while(0)
#	define	THROW2(expr,msg0)		do {if (!(expr)) {string4096	assertion_info; ::Debug.gather_info(_TRE(#expr),msg0,   0,0,DEBUG_INFO,assertion_info); throw assertion_info;}} while(0)
#	define	THROW3(expr,msg0,msg1)	do {if (!(expr)) {string4096	assertion_info; ::Debug.gather_info(_TRE(#expr),msg0,msg1,0,DEBUG_INFO,assertion_info); throw assertion_info;}} while(0)
#else
#	define	THROW					VERIFY
#	define	THROW2					VERIFY2
#	define	THROW3					VERIFY3
#endif

#ifndef _PP_EDITOR_
#pragma warning(disable:4995)
#include "../xrUI/stdafx.h"
#include "../xrScripts/stdafx.h"
#pragma warning(default:4995)

#include "../xrEngine/gamefont.h"
#include "../xrEngine/xr_object.h"
#include "../xrEngine/igame_level.h"
#include "../xrPhysics/xrphysics.h"
#include "smart_cast.h"
#else
#include "../Editors/ActorEditor/stdafx.h"
#endif

#include "../xrEngine/Editor/XrEditorSceneInterface.h"
#include "../xrEngine/AI/game_graph.h"
#include "../xrEngine/AI/game_level_cross_table.h"
#include "../xrEngine/AI/level_graph.h"

#ifndef _EDITOR
#	include "pch_script.h"
extern CInifile* pGameGlobals;
#endif

extern void DestroyImGuiInGame();

#undef min
#undef max