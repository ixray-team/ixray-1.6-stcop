#include "stdafx.h"
#include "GameFont.h"
#pragma hdrstop

#include "../xrcdb/ISpatial.h"
#include "IGame_Persistent.h"
#include "render.h"
#include "xr_object.h"

#include "../Include/xrRender/DrawUtils.h"

int		g_ErrorLineCount	= 15;
Flags32 g_stats_flags		= {0};

