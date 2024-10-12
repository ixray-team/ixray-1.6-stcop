#pragma once
#include <d3d9.h>
using ImTextureID = IDirect3DBaseTexture9*;
#define ImTextureID ImTextureID 

#ifdef XREUI_EXPORTS
#define XREUI_API __declspec(dllexport)
#else
#define XREUI_API __declspec(dllimport)
#endif

#include "..\..\xrCore\xrCore.h"

#include "EditorWnd.h"
#include "XrUIManager.h"

#define IMGUI_API XREUI_API
#define IMGUI_IMPL_API
#define IMGUI_DISABLE_INCLUDE_IMCONFIG_H
#define IMGUI_INCLUDE_IMGUI_USER_H
#define IMGUI_DEFINE_MATH_OPERATORS

#include "imgui.h"