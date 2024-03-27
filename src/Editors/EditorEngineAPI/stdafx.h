#pragma once
#include "../../xrCore/stdafx.h"
#include "../../xrCDB/stdafx.h"

#include <imgui.h>
#include <imgui_internal.h>

class PropValue;
class PropItem;

using PropItemVec = xr_vector<PropItem*>;
using PropItemIt = PropItemVec::iterator;

#include "../../xrCore/ChooseTypes.H"

#define ENGINE_API
#define ECORE_API

#include "../../xrEngine/defines.h"
#include "../../xrEngine/pure.h"

#include "EditorDevice.h"

#include <d3d9.h>

enum EItemType 
{
	TYPE_INVALID = -1,
	TYPE_FOLDER = 0,
	TYPE_OBJECT = 1
};

using AnsiString = xr_string;
using AStringVec = xr_vector<xr_string>;