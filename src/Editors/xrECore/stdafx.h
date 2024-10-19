//----------------------------------------------------
// file: stdafx.h
//----------------------------------------------------
#pragma once
// DirectX headers
#include <d3d9.h>
#include "../xrEUI/stdafx.h"
#include "..\..\Layers\xrRenderDX9\xrD3DDefs.h"

#include "../../utils/xrDXT/xrDXT.h"

#ifdef	XRECORE_EXPORTS
#define ECORE_API		__declspec(dllexport)
#else
#define ECORE_API		__declspec(dllimport)
#endif

#include "..\..\xrEngine\stdafx.h"
#include "..\..\xrEngine\device.h"
#include "..\xrEProps\stdafx.h"
#include "..\..\xrCDB\xrCDB.h"
#include "..\..\xrSound\Sound.h"
#include "..\..\xrParticles\psystem.h"

#include "..\..\XrEngine\fmesh.h"
#include "..\..\XrEngine\_d3d_extensions.h"
#define smart_cast dynamic_cast

#ifndef O_SEQUENTIAL
#define O_SEQUENTIAL 0
#endif

#define DIRECTINPUT_VERSION 0x0800

#define         R_R1    1
#define         R_R2    2
#define         RENDER  R_R1
#define			REDITOR 1



#define PropertyGP(a,b)	__declspec( property( get=a, put=b ) )
#define THROW			FATAL("THROW");
#define THROW2(a)		R_ASSERT(a);
#define clMsg 			Msg

class PropValue;
class PropItem;

using PropItemVec = xr_vector<PropItem*>;
using PropItemIt = PropItemVec::iterator;

class ListItem;

using ListItemsVec = xr_vector<ListItem*>;

// some user components
using AnsiString = xr_string;
using AStringVec = xr_vector<AnsiString>;
using AStringIt = AStringVec::iterator;

using LPAStringVec = xr_vector<AnsiString*>;
using LPAStringIt = LPAStringVec::iterator;


#include "..\Public\xrEProps.h"
#include "..\..\xrCore\Log.h"
#include "editor\ELog.h"
#include "..\..\XrEngine\defines.h"

#include "../../xrphysics/xrphysics.h"
#include "../../Layers\xrRender\FVF.h"

struct str_pred 
{
    IC bool operator()(LPCSTR x, LPCSTR y) const
    {	return strcmp(x,y)<0;	}
};
struct astr_pred
{
    IC bool operator()(const xr_string& x, const xr_string& y) const
    {	return x<y;	}
};

#include "editor\device.h"
#include "..\..\XrEngine\properties.h"
#include "editor\render.h"
using FLvertexVec = xr_vector<FVF::L>;
using FLvertexIt = FLvertexVec::iterator;

using FTLvertexVec = xr_vector<FVF::TL>;
using FTLvertexIt = FTLvertexVec::iterator;

using FLITvertexVec = xr_vector<FVF::LIT>;
using FLITvertexIt = FLITvertexVec::iterator;

using RStrVec = xr_vector<shared_str>;
using RStrVecIt = RStrVec::iterator;

#include "Editor/EditorPreferences.h"

#ifdef _LEVEL_EDITOR                
	#include "../../xrCore/net_utils.h"
#endif

#define INI_NAME(buf) 		{FS.update_path(buf,"$app_data_root$",EFS.ChangeFileExt(UI->EditorName(),".ini").c_str());}
#define JSON_NAME(buf) 		{FS.update_path(buf,"$app_data_root$",EFS.ChangeFileExt(UI->EditorName(),".json").c_str());}
//#define INI_NAME(buf) 		{buf = buf+xr_string(Core.WorkingPath)+xr_string("\\")+EFS.ChangeFileExt(UI->EditorName(),".ini");}
#define DEFINE_INI(storage)	{string_path buf; INI_NAME(buf); storage->IniFileName=buf;}
#define NONE_CAPTION "<none>"
#define MULTIPLESEL_CAPTION "<multiple selection>"

// path definition
#define _server_root_		"$server_root$"
#define _server_data_root_	"$server_data_root$"
#define _local_root_		"$local_root$"
#define _import_			"$import$"
#define _sounds_			"$sounds$"
#define _textures_			"$textures$"
#define _objects_			"$objects$"
#define _maps_				"$maps$"
#define _groups_			"$groups$"
#define _temp_				"$temp$"
#define _omotion_			"$omotion$"
#define _omotions_			"$omotions$"
#define _smotion_			"$smotion$"
#define _detail_objects_	"$detail_objects$"

#define		TEX_POINT_ATT	"internal\\internal_light_attpoint"
#define		TEX_SPOT_ATT	"internal\\internal_light_attclip"

#include "..\..\Layers\xrRender\ETextureParams.h"
#include "..\..\Layers\xrRender\\ResourceManager.h"

#include "../../Layers/xrRender/blenders/Blender_Recorder.h"
#include "../../Layers/xrRender/blenders/Blender.h"
#include "../../Layers/xrRender/blenders/Blender_CLSID.h"

#include "Editor/ImageManager.h"
inline xr_string ChangeFileExt(const char* name, const char* e)
{
	string_path path;
	xr_strcpy(path, name);
	if (strrchr(path,'.'))
	{
		strrchr(path, '.')[0] = 0;
	}
	xr_string str;
	str.append(path);
	str.append(e);
	return str;

}
inline xr_string ChangeFileExt(const xr_string&name, const char* e)
{
	string_path path;
	xr_strcpy(path, name.c_str());
	if (strrchr(path, '.'))
	{
		strrchr(path, '.')[0] = 0;
	}
	xr_string str;
	str.append(path);
	str.append(e);
	return str;

}
inline u32 TColor(u32 r)
{
	return r;
}

#ifdef XRECORE_EXPORTS
inline void not_implemented()
{
	if (IsDebuggerPresent())
		DebugBreak();
	else
	{
		R_ASSERT(0);
	}
}
#endif
