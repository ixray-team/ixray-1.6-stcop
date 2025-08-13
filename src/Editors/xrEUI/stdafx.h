#pragma once

#ifdef XREUI_EXPORTS
#define XREUI_API __declspec(dllexport)
#else
#define XREUI_API __declspec(dllimport)
#endif

#define IM_TEXTURE_RELEASE(tex) ((IDirect3DBaseTexture9*)(tex))->Release()

#include "../../xrCore/xrCore.h"

enum class EDragDropType : u8
{
	Folder,
	File,
	Viewport,
	RandomAppend,
	Details,
	Logic,

	None
};

#include "EditorWnd.h"
#include "XrUIManager.h"

#define IMGUI_IMPL_API
#define IMGUI_DISABLE_INCLUDE_IMCONFIG_H
#define IMGUI_DEFINE_MATH_OPERATORS

#include "imgui.h"

namespace ImGui
{
	XREUI_API bool HyperLink(const char* label, const char* url, bool underlineWhenHoveredOnly = false);
}